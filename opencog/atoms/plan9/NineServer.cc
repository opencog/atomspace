/*
 * opencog/atoms/plan9/NineServer.cc
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

#include <opencog/atoms/plan9/NineServer.h>

#include <algorithm>
#include <stdexcept>

namespace opencog
{
namespace plan9
{

NineServer::NineServer(const NineNamespace& ns) : _ns(ns)
{}

std::string NineServer::parent_path(const std::string& path)
{
	std::string canonical = NineNamespace::canon(path);
	size_t slash = canonical.find_last_of('/');
	if (slash == std::string::npos or slash == 0)
		return "/";
	return canonical.substr(0, slash);
}

std::string NineServer::join_path(const std::string& path, const std::string& c)
{
	if (path == "/") return "/" + c;
	return path + "/" + c;
}

NineQid NineServer::top_qid(const std::vector<NineNodePtr>& stack)
{
	if (stack.empty() or not stack.front())
	{
		NineQid q; q.type = QTFILE; q.path = 0; return q;
	}
	return stack.front()->qid();
}

bool NineServer::stack_is_dir(const std::vector<NineNodePtr>& stack)
{
	return (not stack.empty()) and stack.front() and stack.front()->is_dir();
}

NineStat NineServer::stat_for(const std::string& path,
                               const std::vector<NineNodePtr>& stack) const
{
	NineStat st;
	st.qid = top_qid(stack);
	bool is_dir = stack_is_dir(stack);
	st.mode = is_dir ? (NINE_DMDIR | 0555u) : 0444u;
	if (not stack.empty() and stack.front() and stack.front()->is_writable())
		st.mode |= 0222u;
	st.length = is_dir ? 0 : (stack.empty() ? 0 : stack.front()->length());
	st.uid = "atomspace";
	st.gid = "atomspace";
	st.muid = "atomspace";

	if (path == "/")
	{
		st.name = "/";
	}
	else
	{
		size_t slash = path.find_last_of('/');
		st.name = (slash == std::string::npos) ? path : path.substr(slash + 1);
	}
	return st;
}

NineMsg NineServer::dispatch(const NineMsg& req)
{
	try
	{
		switch (req.type)
		{
			case Tversion: return do_version(req);
			case Tauth:    return nine_error(req.tag,
				"authentication not required");
			case Tattach:  return do_attach(req);
			case Tflush:   return do_flush(req);
			case Twalk:    return do_walk(req);
			case Topen:    return do_open(req);
			case Tcreate:  return nine_error(req.tag,
				"create not supported; write to /new instead");
			case Tread:    return do_read(req);
			case Twrite:   return do_write(req);
			case Tclunk:   return do_clunk(req);
			case Tremove:  return do_remove(req);
			case Tstat:    return do_stat(req);
			case Twstat:   return nine_error(req.tag, "read-only namespace");
			default:
				return nine_error(req.tag, "message type not implemented");
		}
	}
	catch (const std::exception& e)
	{
		return nine_error(req.tag, e.what());
	}
}

std::string NineServer::handle_message(const std::string& request)
{
	NineMsg req;
	size_t used = nine_decode(request, req);
	if (0 == used)
		throw std::runtime_error("9P: incomplete message");
	NineMsg rep = dispatch(req);
	return nine_encode(rep);
}

NineMsg NineServer::do_version(const NineMsg& req)
{
	NineMsg rep;
	rep.type = Rversion;
	rep.tag = req.tag;
	_msize = std::min(req.msize, NINE_DEFAULT_MSIZE);
	rep.msize = _msize;
	rep.version = (req.version.substr(0, 6) == "9P2000")
		? std::string(NINE_VERSION) : std::string("unknown");
	return rep;
}

NineMsg NineServer::do_attach(const NineMsg& req)
{
	std::string path = req.aname.empty() ? "/" : NineNamespace::canon(req.aname);
	std::vector<NineNodePtr> stack = _ns.walk_union(path);
	if (stack.empty())
		return nine_error(req.tag, "attach: no such path: " + req.aname);

	_fids[req.fid] = NineFid{ path, stack, 0, false };

	NineMsg rep;
	rep.type = Rattach;
	rep.tag = req.tag;
	rep.qid = top_qid(stack);
	return rep;
}

NineMsg NineServer::do_walk(const NineMsg& req)
{
	auto it = _fids.find(req.fid);
	if (it == _fids.end())
		return nine_error(req.tag, "walk: unknown fid");

	std::string path = it->second.path;
	std::vector<NineNodePtr> stack = it->second.stack;
	std::vector<NineQid> qids;

	for (const auto& comp : req.wname)
	{
		std::string candidate =
			(comp == "..") ? parent_path(path) : join_path(path, comp);
		std::vector<NineNodePtr> next = _ns.walk_union(candidate);
		if (next.empty())
			break; // Partial walk; per 9P this is not itself an error.
		path = candidate;
		stack = next;
		qids.push_back(top_qid(stack));
	}

	if (not req.wname.empty() and qids.empty())
		return nine_error(req.tag, "walk: file not found");

	NineMsg rep;
	rep.type = Rwalk;
	rep.tag = req.tag;
	rep.wqid = qids;

	// A full (or trivially empty, i.e. clone) walk commits to newfid;
	// a partial walk reports what it found without creating newfid,
	// matching walk(5)'s semantics.
	if (qids.size() == req.wname.size())
		_fids[req.newfid] = NineFid{ path, stack, 0, false };

	return rep;
}

NineMsg NineServer::do_open(const NineMsg& req)
{
	auto it = _fids.find(req.fid);
	if (it == _fids.end())
		return nine_error(req.tag, "open: unknown fid");

	uint8_t rw = req.mode & 3;
	if ((rw == OWRITE or rw == ORDWR) and
	    not (not it->second.stack.empty() and it->second.stack.front() and
	         it->second.stack.front()->is_writable()))
	{
		return nine_error(req.tag, "open: permission denied");
	}

	it->second.opened = true;
	it->second.mode = req.mode;

	NineMsg rep;
	rep.type = Ropen;
	rep.tag = req.tag;
	rep.qid = top_qid(it->second.stack);
	rep.iounit = 0;
	return rep;
}

NineMsg NineServer::do_read(const NineMsg& req)
{
	auto it = _fids.find(req.fid);
	if (it == _fids.end())
		return nine_error(req.tag, "read: unknown fid");
	if (not it->second.opened)
		return nine_error(req.tag, "read: fid not open");

	NineMsg rep;
	rep.type = Rread;
	rep.tag = req.tag;

	if (stack_is_dir(it->second.stack))
	{
		// Simplified directory read: the whole listing is produced
		// on the first read (offset 0); subsequent reads at nonzero
		// offset return EOF. This avoids needing to track a stable
		// per-fid cursor across separate Tread calls, at the cost of
		// not supporting incremental paging of very large
		// directories -- acceptable for a synthetic, introspection
		// filesystem, and documented in README.md.
		if (req.offset != 0)
		{
			rep.data.clear();
			return rep;
		}

		std::vector<NineNodePtr> kids = _ns.list(it->second.path);
		std::string blob;
		for (const auto& k : kids)
		{
			if (not k) continue;
			std::string childpath = join_path(it->second.path, k->name());
			std::vector<NineNodePtr> one = { k };
			std::string entry = nine_encode_stat(stat_for(childpath, one));
			if (blob.size() + entry.size() > req.count)
				break;
			blob += entry;
		}
		rep.data = blob;
		return rep;
	}

	NineNodePtr file = it->second.stack.front();
	rep.data = file ? file->read(req.offset, req.count) : std::string();
	return rep;
}

NineMsg NineServer::do_write(const NineMsg& req)
{
	auto it = _fids.find(req.fid);
	if (it == _fids.end())
		return nine_error(req.tag, "write: unknown fid");
	if (not it->second.opened)
		return nine_error(req.tag, "write: fid not open");
	if (stack_is_dir(it->second.stack))
		return nine_error(req.tag, "write: is a directory");

	NineNodePtr file = it->second.stack.front();
	uint32_t n = file ? file->write(req.offset, req.data) : 0;

	NineMsg rep;
	rep.type = Rwrite;
	rep.tag = req.tag;
	rep.count = n;
	return rep;
}

NineMsg NineServer::do_clunk(const NineMsg& req)
{
	_fids.erase(req.fid);
	NineMsg rep;
	rep.type = Rclunk;
	rep.tag = req.tag;
	return rep;
}

NineMsg NineServer::do_remove(const NineMsg& req)
{
	// This namespace is structurally read-only (atoms/dirs cannot be
	// deleted through it); Tremove still clunks the fid, per remove(5)
	// ("whether or not the remove succeeds, the fid is clunked").
	_fids.erase(req.fid);
	NineMsg rep;
	rep.type = Rremove;
	rep.tag = req.tag;
	return rep;
}

NineMsg NineServer::do_stat(const NineMsg& req)
{
	auto it = _fids.find(req.fid);
	if (it == _fids.end())
		return nine_error(req.tag, "stat: unknown fid");

	NineMsg rep;
	rep.type = Rstat;
	rep.tag = req.tag;
	rep.stat = nine_encode_stat(stat_for(it->second.path, it->second.stack));
	return rep;
}

NineMsg NineServer::do_flush(const NineMsg& req)
{
	// This server is strictly synchronous (dispatch() never blocks
	// past the single request being handled), so there is never an
	// in-flight request to actually cancel; acknowledge immediately.
	NineMsg rep;
	rep.type = Rflush;
	rep.tag = req.tag;
	return rep;
}

} // namespace plan9
} // namespace opencog
