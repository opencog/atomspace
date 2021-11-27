/*
 * opencog/atomspace/TypeIndex.h
 *
 * Copyright (C) 2008 Linas Vepstas <linasvepstas@gmail.com>
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
#include <vector>

#if HAVE_FOLLY
#include <folly/container/F14Set.h>
#else
#include <set>
#endif

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

#if HAVE_FOLLY
typedef folly::F14ValueSet<Handle> AtomSet;
#else
typedef std::unordered_set<Handle> AtomSet;
#endif

#define TYPE_INDEX_SHARED_LOCK std::shared_lock<std::shared_mutex> lck(_mtx);
#define TYPE_INDEX_UNIQUE_LOCK std::unique_lock<std::shared_mutex> lck(_mtx);

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
		std::vector<AtomSet> _idx;
		size_t _num_types;
		NameServer& _nameserver;

		// Single, global mutex for locking the index.
		mutable std::shared_mutex _mtx;
	public:
		TypeIndex(void);
		void resize(void);

		// Return a Handle, if it's already in the set.
		// Else, return nullptr
		Handle insertAtom(const Handle& h)
		{
			AtomSet& s(_idx.at(h->get_type()));
			TYPE_INDEX_UNIQUE_LOCK;
			auto iter = s.find(h);
			if (s.end() != iter) return *iter;
			s.insert(h);
			return Handle::UNDEFINED;
		}

		void removeAtom(const Handle& h)
		{
			AtomSet& s(_idx.at(h->get_type()));
			TYPE_INDEX_UNIQUE_LOCK;
			s.erase(h);
		}

		Handle findAtom(const Handle& h) const
		{
			const AtomSet& s(_idx.at(h->get_type()));
			TYPE_INDEX_SHARED_LOCK;
			auto iter = s.find(h);
			if (s.end() == iter) return Handle::UNDEFINED;
			return *iter;
		}

		// How many atoms are ther of type t?
		size_t size(Type t) const
		{
			const AtomSet& s(_idx.at(t));
			TYPE_INDEX_SHARED_LOCK;
			return s.size();
		}

		// How many atoms, grand total?
		size_t size(void) const
		{
			size_t cnt = 0;
			TYPE_INDEX_SHARED_LOCK;
			for (const auto& s : _idx)
				cnt += s.size();
			return cnt;
		}

		// How many atoms, of type t, and subclasses also?
		size_t size(Type type, bool subclass) const
		{
			size_t result = size(type);
			if (not subclass) return result;

			for (Type t = ATOM; t<_num_types; t++)
			{
				if (t != type and _nameserver.isA(t, type))
					result += size(t);
			}
			return result;
		}

		void clear(void)
		{
			TYPE_INDEX_UNIQUE_LOCK;
			for (auto& s : _idx)
			{
				for (auto& h : s)
				{
					h->_atom_space = nullptr;

					// We installed the incoming set; we remove it too.
					h->remove();
				}
				s.clear();
			}
		}

		void get_handles_by_type(HandleSeq&, Type, bool subclass) const;
		void get_rootset_by_type(HandleSeq&, Type, bool subclass,
		                         const AtomSpace*) const;
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_TYPEINDEX_H
