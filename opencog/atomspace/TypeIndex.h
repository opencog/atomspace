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

#include <set>
#include <vector>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>

class AtomSpaceUTest;

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef std::unordered_set<Handle> AtomSet;

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
	friend class ::AtomSpaceUTest;

	private:
		std::vector<AtomSet> _idx;
		size_t _num_types;
	public:
		TypeIndex(void);
		void resize(void);
		void insertAtom(const Handle& h)
		{
			AtomSet& s(_idx.at(h->get_type()));
			s.insert(h);
		}
		void removeAtom(const Handle& h)
		{
			AtomSet& s(_idx.at(h->get_type()));
			s.erase(h);
		}

		Handle findAtom(const Handle& h) const
		{
			const AtomSet& s(_idx.at(h->get_type()));
			auto iter = s.find(h);
			if (s.end() == iter) return Handle::UNDEFINED;
			return *iter;
		}

		size_t size(Type t) const
		{
			const AtomSet& s(_idx.at(t));
			return s.size();
		}

		size_t size(void) const
		{
			size_t cnt = 0;
			for (const auto& s : _idx)
				cnt += s.size();
			return cnt;
		}

		void clear(void)
		{
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

		// Return true if there exists some index containing duplicated
		// atoms (equal by content). Used during unit tests.
		bool contains_duplicate() const;
		bool contains_duplicate(const AtomSet& atoms) const;

		class iterator
			: public HandleIterator
		{
			friend class TypeIndex;
			public:
				iterator(Type, bool);
				iterator& operator++();
				iterator& operator++(int);
				iterator& operator=(iterator);
				bool operator==(iterator);
				bool operator!=(iterator);
				Handle operator*(void);
			private:
				Type type;
				bool subclass;
				std::vector<AtomSet>::const_iterator s;
				std::vector<AtomSet>::const_iterator send;
				Type currtype;
				AtomSet::const_iterator se;
		};

		iterator begin(Type, bool) const;
		iterator end(void) const;
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_TYPEINDEX_H
