/*
 * opencog/atoms/plan9/AtomFs.cc
 *
 * Copyright (c) 2024 OpenCog Foundation
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/plan9/AtomFs.h>

#include <cstdio>
#include <cstdlib>
#include <functional>
#include <sstream>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/value/FloatValue.h>

#ifdef HAVE_GUILE
#include <opencog/guile/SchemeEval.h>
#endif

namespace opencog
{
namespace plan9
{

// ---------------------------------------------------------------
// Small helpers.

static uint64_t mix_id(uint64_t base, const std::string& salt)
{
	// A boost::hash_combine-style mixer: gives distinct, deterministic
	// Qid.path values for the several synthetic entries hung off of a
	// single atom (name, type, outgoing/, ...) without colliding with
	// the atom's own directory Qid (which uses `base` unmixed).
	uint64_t h2 = (uint64_t) std::hash<std::string>{}(salt);
	return base ^ (h2 + 0x9e3779b97f4a7c15ULL + (base << 6) + (base >> 2));
}

std::string AtomFsIndex::id_of(const Handle& h)
{
	char buf[20];
	snprintf(buf, sizeof(buf), "%016llx",
		(unsigned long long) h.value());
	return std::string(buf);
}

// ---------------------------------------------------------------
// Simple read-only text file backed by a content-generating functor,
// re-evaluated on every read so it always reflects live AtomSpace
// state.

namespace {

class TextFileNode : public NineNode
{
public:
	TextFileNode(std::string name, uint64_t qid_path,
	             std::function<std::string()> content) :
		_name(std::move(name)), _qid_path(qid_path),
		_content(std::move(content))
	{}

	std::string name() const override { return _name; }
	bool is_dir() const override { return false; }
	NineQid qid() const override
	{
		NineQid q; q.type = QTFILE; q.path = _qid_path; return q;
	}
	uint64_t length() const override { return _content().size(); }
	std::string read(uint64_t offset, uint32_t count) const override
	{
		std::string s = _content();
		if (offset >= s.size()) return std::string();
		return s.substr(offset, count);
	}

private:
	std::string _name;
	uint64_t _qid_path;
	std::function<std::string()> _content;
};

/// A generic directory node whose children are supplied by a functor,
/// evaluated fresh on every children()/lookup() call.
class FuncDirNode : public NineNode
{
public:
	FuncDirNode(std::string name, uint64_t qid_path,
	            std::function<std::vector<NineNodePtr>()> lister) :
		_name(std::move(name)), _qid_path(qid_path),
		_lister(std::move(lister))
	{}

	std::string name() const override { return _name; }
	bool is_dir() const override { return true; }
	NineQid qid() const override
	{
		NineQid q; q.type = QTDIR; q.path = _qid_path; return q;
	}
	std::vector<NineNodePtr> children() const override { return _lister(); }

private:
	std::string _name;
	uint64_t _qid_path;
	std::function<std::vector<NineNodePtr>()> _lister;
};

/// The truth-value convenience file: reads back "<strength>
/// <confidence>" from the atom's truth_key() value (when it is a
/// FloatValue with at least two components), and, when the
/// AtomSpace is writable, accepts writes of the same format to
/// replace it.
class TvFileNode : public NineNode
{
public:
	TvFileNode(Handle h, uint64_t qid_path) :
		_h(std::move(h)), _qid_path(qid_path)
	{}

	std::string name() const override { return "tv"; }
	bool is_dir() const override { return false; }
	NineQid qid() const override
	{
		NineQid q; q.type = QTFILE; q.path = _qid_path; return q;
	}
	bool is_writable() const override
	{
		AtomSpace* as = _h->getAtomSpace();
		return nullptr != as and not as->get_read_only();
	}
	std::string read(uint64_t offset, uint32_t count) const override
	{
		std::string s = render();
		if (offset >= s.size()) return std::string();
		return s.substr(offset, count);
	}
	uint32_t write(uint64_t offset, const std::string& data) override
	{
		if (offset != 0 or not is_writable())
			return 0;
		double s = 1.0, c = 0.0;
		if (2 != sscanf(data.c_str(), "%lf %lf", &s, &c))
			return 0;
		_h->setValue(truth_key(),
			createFloatValue(std::vector<double>({s, c})));
		return (uint32_t) data.size();
	}

private:
	std::string render() const
	{
		FloatValuePtr fv = FloatValueCast(_h->getValue(truth_key()));
		if (nullptr == fv or fv->value().size() < 2)
			return "1 0\n";
		std::ostringstream oss;
		oss << fv->value()[0] << " " << fv->value()[1] << "\n";
		return oss.str();
	}

	Handle _h;
	uint64_t _qid_path;
};

/// The "new" control file: writing an Atomese (Scheme) expression
/// evaluates it in the AtomSpace's evaluator and records the
/// resulting atom's /by-handle path for a subsequent read. Requires
/// this library to have been built with Guile support; otherwise,
/// writes are accepted but report that limitation back on read.
class NewControlNode : public NineNode
{
public:
	explicit NewControlNode(AtomFsIndex* index) : _index(index) {}

	std::string name() const override { return "new"; }
	bool is_dir() const override { return false; }
	NineQid qid() const override
	{
		NineQid q; q.type = QTFILE; q.path = mix_id(0, "new-control"); return q;
	}
	bool is_writable() const override { return true; }
	std::string read(uint64_t offset, uint32_t count) const override
	{
		if (offset >= _last_reply.size()) return std::string();
		return _last_reply.substr(offset, count);
	}
	uint32_t write(uint64_t offset, const std::string& data) override
	{
		(void) offset;
#ifdef HAVE_GUILE
		SchemeEval* eval =
			SchemeEval::get_scheme_evaluator(_index->atomspace().get());
		Handle h = eval->eval_h(data);
		if (nullptr == h)
			_last_reply = "error: evaluation did not yield an atom\n";
		else
			_last_reply = "/by-handle/" + AtomFsIndex::id_of(h) + "\n";
#else
		_last_reply = "error: built without Guile support\n";
#endif
		return (uint32_t) data.size();
	}

private:
	AtomFsIndex* _index;
	std::string _last_reply;
};

/// A named alias for another node: same Qid, children, and
/// read/write behaviour as `_target`, but a different directory-entry
/// name. This is how outgoing/incoming "hard links" are implemented:
/// the exact same underlying AtomDir node is reachable under several
/// different names (a positional index under outgoing/, a content-id
/// under incoming/ or by-handle/), and Qid equality (not object
/// identity of the wrapper) is what makes them the "same file".
class AliasNode : public NineNode
{
public:
	AliasNode(std::string name, NineNodePtr target) :
		_name(std::move(name)), _target(std::move(target))
	{}

	std::string name() const override { return _name; }
	bool is_dir() const override { return _target->is_dir(); }
	NineQid qid() const override { return _target->qid(); }
	uint64_t length() const override { return _target->length(); }
	std::vector<NineNodePtr> children() const override
	{
		return _target->children();
	}
	NineNodePtr lookup(const std::string& c) const override
	{
		return _target->lookup(c);
	}
	std::string read(uint64_t offset, uint32_t count) const override
	{
		return _target->read(offset, count);
	}
	uint32_t write(uint64_t offset, const std::string& data) override
	{
		return _target->write(offset, data);
	}
	bool is_writable() const override { return _target->is_writable(); }

private:
	std::string _name;
	NineNodePtr _target;
};

} // anonymous namespace

// ---------------------------------------------------------------
// AtomFsIndex: builds and memoizes the per-atom directory tree.

AtomFsIndex::AtomFsIndex(const AtomSpacePtr& as, SheafHookPtr hook) :
	_as(as), _hook(std::move(hook))
{
	AtomFsIndex* self = this;

	NineNodePtr new_ctl(new NewControlNode(self));

	NineNodePtr by_handle(new FuncDirNode("by-handle", mix_id(0, "by-handle"),
		[self]() -> std::vector<NineNodePtr>
		{
			HandleSeq all;
			self->_as->get_handles_by_type(all, ATOM, true);
			std::vector<NineNodePtr> out;
			out.reserve(all.size());
			for (const Handle& h : all)
				out.push_back(self->dir_for(h));
			return out;
		}));

	NineNodePtr by_type(new FuncDirNode("by-type", mix_id(0, "by-type"),
		[self]() -> std::vector<NineNodePtr>
		{
			HandleSeq all;
			self->_as->get_handles_by_type(all, ATOM, true);
			std::vector<Type> seen;
			std::vector<NineNodePtr> out;
			for (const Handle& h : all)
			{
				Type t = h->get_type();
				bool dup = false;
				for (Type s : seen) if (s == t) { dup = true; break; }
				if (dup) continue;
				seen.push_back(t);

				const std::string tname = nameserver().getTypeName(t);
				out.push_back(NineNodePtr(new FuncDirNode(
					tname, mix_id((uint64_t) t, "by-type-entry"),
					[self, t]() -> std::vector<NineNodePtr>
					{
						HandleSeq hs;
						self->_as->get_handles_by_type(hs, t, false);
						std::vector<NineNodePtr> kids;
						kids.reserve(hs.size());
						for (const Handle& h : hs)
							kids.push_back(self->dir_for(h));
						return kids;
					})));
			}
			return out;
		}));

	_root = NineNodePtr(new FuncDirNode("/", mix_id(0, "root"),
		[by_type, by_handle, new_ctl]() -> std::vector<NineNodePtr>
		{
			return { by_type, by_handle, new_ctl };
		}));
}

NineNodePtr AtomFsIndex::dir_for(const Handle& h)
{
	std::lock_guard<std::mutex> lock(_mtx);

	ContentHash key = h.value();
	auto it = _cache.find(key);
	if (it != _cache.end())
		return it->second;

	AtomFsIndex* self = this;
	Handle atom = h;
	const uint64_t qbase = key;
	const std::string id = id_of(h);

	NineNodePtr name_file(new TextFileNode("name", mix_id(qbase, "name"),
		[atom]() { return atom->get_name() + "\n"; }));

	NineNodePtr type_file(new TextFileNode("type", mix_id(qbase, "type"),
		[atom]() { return nameserver().getTypeName(atom->get_type()) + "\n"; }));

	NineNodePtr to_string_file(new TextFileNode("to_string",
		mix_id(qbase, "to_string"),
		[atom]() { return atom->to_string() + "\n"; }));

	NineNodePtr tv_file(new TvFileNode(atom, mix_id(qbase, "tv")));

	NineNodePtr outgoing_dir(new FuncDirNode("outgoing",
		mix_id(qbase, "outgoing"),
		[self, atom]() -> std::vector<NineNodePtr>
		{
			std::vector<NineNodePtr> out;
			const HandleSeq& oset = atom->getOutgoingSet();
			out.reserve(oset.size());
			for (size_t i = 0; i < oset.size(); i++)
			{
				out.push_back(NineNodePtr(new AliasNode(
					std::to_string(i), self->dir_for(oset[i]))));
			}
			return out;
		}));

	NineNodePtr incoming_dir(new FuncDirNode("incoming",
		mix_id(qbase, "incoming"),
		[self, atom]() -> std::vector<NineNodePtr>
		{
			std::vector<NineNodePtr> out;
			IncomingSet iset = atom->getIncomingSet();
			out.reserve(iset.size());
			for (const Handle& ih : iset)
			{
				out.push_back(NineNodePtr(new AliasNode(
					AtomFsIndex::id_of(ih), self->dir_for(ih))));
			}
			return out;
		}));

	NineNodePtr values_dir(new FuncDirNode("values",
		mix_id(qbase, "values"),
		[atom]() -> std::vector<NineNodePtr>
		{
			std::vector<NineNodePtr> out;
			HandleSet keys = atom->getKeys();
			for (const Handle& key : keys)
			{
				ValuePtr v = atom->getValue(key);
				std::string label = AtomFsIndex::id_of(key);
				out.push_back(NineNodePtr(new TextFileNode(label,
					mix_id(key.value(), "value-entry"),
					[key, v]()
					{
						return key->to_short_string() + " = " +
							(v ? v->to_string() : std::string("()")) + "\n";
					})));
			}
			return out;
		}));

	std::vector<NineNodePtr> kids =
		{ name_file, type_file, to_string_file, tv_file,
		  outgoing_dir, incoming_dir, values_dir };

	SheafHookPtr hook = _hook;
	if (hook)
	{
		std::vector<std::string> lines = hook->connectors(atom);
		if (not lines.empty())
		{
			std::string joined;
			for (const auto& l : lines) joined += l + "\n";
			size_t n = lines.size();

			NineNodePtr connectors_file(new TextFileNode("connectors",
				mix_id(qbase, "sheaf-connectors"),
				[joined]() { return joined; }));
			NineNodePtr count_file(new TextFileNode("count",
				mix_id(qbase, "sheaf-count"),
				[n]() { return std::to_string(n) + "\n"; }));

			kids.push_back(NineNodePtr(new FuncDirNode("sheaf",
				mix_id(qbase, "sheaf"),
				[connectors_file, count_file]() -> std::vector<NineNodePtr>
				{
					return { connectors_file, count_file };
				})));
		}
	}

	NineNodePtr dir(new FuncDirNode(id, qbase,
		[kids]() -> std::vector<NineNodePtr> { return kids; }));

	_cache[key] = dir;
	return dir;
}

bool AtomFsIndex::handle_for_id(const std::string& id, Handle& h) const
{
	HandleSeq all;
	_as->get_handles_by_type(all, ATOM, true);
	for (const Handle& cand : all)
	{
		if (id_of(cand) == id)
		{
			h = cand;
			return true;
		}
	}
	return false;
}

} // namespace plan9
} // namespace opencog
