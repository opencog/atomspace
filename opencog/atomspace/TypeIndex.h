/*
 * opencog/atomspace/TypeIndex.h
 *
 * Copyright (C) 2008,2025 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_TYPEINDEX_H
#define _OPENCOG_TYPEINDEX_H

#include <mutex>
#include <set>
#include <vector>

#if HAVE_FOLLY
#include <folly/container/F14Set.h>
#endif

#if HAVE_SPARSEHASH
#include <sparsehash/sparse_hash_set>
#endif

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

// Facebook Folly
// https://github.com/facebook/folly/blob/main/folly/container/F14.md
// promises a faster and more compact hash table. Just two problems:
// 1) It does not make any difference in the "real world" benchmark
//    I tried (the `bio-loop3.scm` benchmark from opencog/benchmark)
// 2) It passes all unit tests, but one: sexpr-query-test which
//    sometimes passes, sometimes fails, because the pattern matcher
//    sometimes reports the same result twice. Why? I dunno. This
//    one failure is enough to say "not recommended." I don't need
//    to be chasing obscure bugs.
#if USE_FOLLY
	typedef folly::F14ValueSet<Handle> AtomHanSet;
#endif

// DO NOT ENABLE sparshash for the TypeSet!!
// #define USE_SPARSE_TYPESET 1
//
// sparsehash
// -- Size of Atom shrinks by 24 Bytes; this is the size of the
//    std::shared_ptr<> in Handle. Empty buckets would normally have
//    one of these; here, there are zero.
// -- addNode performce is 4x slower.
// -- addLink performance is 3x slower.
// Above are measured using `atomspace_bm` from
// https://github.com/opencog/benchmark.git
// This total collapse in performance screams: NO!!!
//
#if USE_SPARSE_TYPESET
	typedef google::sparse_hash_set<Handle> AtomHanSet;
#endif

#if not (USE_SPARSE_TYPESET || USE_FOLLY)
	typedef std::unordered_set<Handle> AtomHanSet;
#endif

// The AtomSet is just a set, plus a lock on that set.
struct AtomSet : AtomHanSet
{
	mutable std::shared_mutex _mtx;
#if USE_SPARSE_TYPESET
	AtomSet() { set_deleted_key(Handle()); }
#else
	AtomSet() = default;
#endif
	AtomSet(AtomSet&& other) noexcept :
		AtomHanSet(std::move(other))
	{}
};

#define TYPE_INDEX_SHARED_LOCK(s) std::shared_lock<std::shared_mutex> lck(s._mtx);
#define TYPE_INDEX_UNIQUE_LOCK(s) std::unique_lock<std::shared_mutex> lck(s._mtx);

/**
 * Implements a vector of AtomSets; each AtomSet is a hash table of
 * Atom pointers.  Thus, given an Atom Type, this can quickly find
 * all of the Atoms of that Type.
 *
 * The primary interface for this is an iterator, and that is because
 * the index will typically contain millions of atoms, and this is far
 * too much to try to copy into some temporary array.  Iterating is much
 * faster.
 *
 * @todo The iterator is NOT thread-safe against the insertion or
 * removal of atoms!  Either inserting or removing an atom will cause
 * the iterator references to be freed, leading to mystery crashes!
 */
class TypeIndex
{
	private:
		mutable int _num_types;
		mutable int _reserved;
		int _offset_to_atom;
		NameServer& _nameserver;
		mutable std::vector<AtomSet> _idx;

		static constexpr int TYPE_RESERVE_SIZE = 1024;
		static constexpr int POOL_SIZE = 8;
		static constexpr int VEC_SIZE = TYPE_RESERVE_SIZE * POOL_SIZE;
		int get_bucket_start(Type t) const
		{
			OC_ASSERT(_offset_to_atom <= t, "BUG with type buckets!");
			if (_reserved + _offset_to_atom <= t) resize();
			return POOL_SIZE * (t - _offset_to_atom);
		}
		int get_bucket(const Handle& h) const
		{
			int ibu = h->get_hash() % POOL_SIZE;
			Type hty = h->get_type();
			if (_reserved + _offset_to_atom <= hty) resize();
			ibu += POOL_SIZE * (hty - _offset_to_atom);
			return ibu;
		}
		AtomSet& get_atom_set(const Handle& h)
		{
			return _idx[get_bucket(h)];
		}
		const AtomSet& get_atom_set_const(const Handle& h) const
		{
			return _idx[get_bucket(h)];
		}
	public:
		TypeIndex(void);
		void resize(void) const;

		// Return a Handle, if it's already in the set.
		// Else, return nullptr
		Handle insertAtom(const Handle& h)
		{
			AtomSet& s(get_atom_set(h));
			TYPE_INDEX_UNIQUE_LOCK(s);
			auto iter = s.find(h);
			if (s.end() != iter) return *iter;
			s.insert(h);
			return Handle::UNDEFINED;
		}

		bool removeAtom(const Handle& h)
		{
			AtomSet& s(get_atom_set(h));
			TYPE_INDEX_UNIQUE_LOCK(s);
			return 1 == s.erase(h);
		}

		Handle findAtom(const Handle& h) const
		{
			const AtomSet& s(get_atom_set_const(h));
			TYPE_INDEX_SHARED_LOCK(s);
			auto iter = s.find(h);
			if (s.end() == iter) return Handle::UNDEFINED;
			return *iter;
		}

		// How many atoms are there of type t?
		size_t size(Type t) const
		{
			if (t < _offset_to_atom) return 0;
			size_t cnt = 0;
			int start = get_bucket_start(t);
			for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
			{
				const AtomSet& s(_idx[ibu]);
				TYPE_INDEX_SHARED_LOCK(s);
				cnt += s.size();
			}
			return cnt;
		}

		// How many atoms, grand total?
		size_t size(void) const
		{
			size_t cnt = 0;
			for (const auto& s : _idx)
			{
				TYPE_INDEX_SHARED_LOCK(s);
				cnt += s.size();
			}
			return cnt;
		}

		// How many atoms, of type t, and subclasses also?
		size_t size(Type type, bool subclass) const
		{
			size_t result = 0;
			if (_offset_to_atom <= type)
				result = size(type);
			if (not subclass) return result;

			// All subclassed types have a larger type.
			for (Type t = type+1; t<_num_types; t++)
			{
				if (t != type and _nameserver.isA(t, type))
					result += size(t);
			}
			return result;
		}

		void clear(void);

		void get_handles_by_type(HandleSeq&, Type, bool subclass) const;
		void get_handles_by_type(UnorderedHandleSet&, Type, bool subclass) const;
		void get_rootset_by_type(HandleSeq&, Type, bool subclass,
		                         const AtomSpace*) const;
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_TYPEINDEX_H
