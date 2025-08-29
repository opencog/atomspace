/*
 * opencog/atomspace/TypeIndex.cc
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

#include "TypeIndex.h"
#include <opencog/atoms/atom_types/NameServer.h>

using namespace opencog;

TypeIndex::TypeIndex(void) :
	_reserved(TYPE_RESERVE_SIZE),
	_nameserver(nameserver()),
	_idx(VEC_SIZE)
{
	_num_types = nameserver().getNumberOfClasses();
	_offset_to_atom = ATOM;
	resize();
}

#define GET_BFL(vec) \
	for (int ibu = 0; ibu < (int) vec.size(); ibu++) { \
		AtomSet& s(vec[ibu]); \
		s._mtx.lock(); }

#define DROP_BFL(vec) \
	for (int ibu = 0; ibu < (int) vec.size(); ibu++) { \
		AtomSet& s(vec[ibu]); \
		s._mtx.unlock(); }

void TypeIndex::resize(void) const
{
	int newsz = nameserver().getNumberOfClasses();
	if (newsz < _reserved + _offset_to_atom) return;

	// If we are here, we need to resize. Get the BFL.

	while (_reserved + _offset_to_atom < newsz)
		_reserved *= 2;

	std::vector<AtomSet> newvec(_reserved * POOL_SIZE);
	GET_BFL(_idx)
	newvec.swap(_idx);
	_num_types = newsz;
	DROP_BFL(newvec)
}

void TypeIndex::clear(void)
{
	std::vector<AtomSet> dead(_reserved * POOL_SIZE);
	GET_BFL(_idx)
	dead.swap(_idx);

	// Clear the AtomSpace before releasing the lock.
	for (auto& s : dead)
	{
		for (auto& h : s)
			h->_atom_space = nullptr;
	}

	DROP_BFL(dead)

	// Do the final cleanup after releasing the lock. This enables
	// the very unlikely situation of having other threads start
	// using the AtomSpace again, while we do final cleanup. BTW,
	// the matching `install()` for the `remove()` below happened
	// in the `AtomSpace::add()` method. We do it here cause its
	// easier. Anyway, we can't do the `remove()` under the lock,
	// that would result in lock inversion.
	for (auto& s : dead)
	{
		for (auto& h : s)
			h->remove();
		s.clear();
	}
}

// ================================================================

void TypeIndex::get_handles_by_type(HandleSeq& hseq,
                                    Type type,
                                    bool subclass) const
{
	if (not subclass and type < _offset_to_atom) return;

	// Get the initial size of the handles vector.
	size_t initial_size = hseq.size();

	// Determine the number of atoms we'll be adding.
	size_t size_of_append = size(type, subclass);

	// Now reserve size for the addition. This is faster for large
	// append iterations since appends to the list won't require new
	// allocations and copies whenever the allocated size is exceeded.
	hseq.reserve(initial_size + size_of_append);

	if (type >= _offset_to_atom)
	{
		int start = get_bucket_start(type);
		for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
		{
			const AtomSet& s(_idx[ibu]);
			TYPE_INDEX_SHARED_LOCK(s);
			for (const Handle& h : s)
				hseq.push_back(h);
		}
	}

	// Not subclassing? We are done!
	if (not subclass) return;

	Type tstar = std::max(type+1, _offset_to_atom);
	for (Type t = tstar; t<_num_types; t++)
	{
		if (not _nameserver.isA(t, type)) continue;

		int start = get_bucket_start(t);
		for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
		{
			const AtomSet& s(_idx[ibu]);
			TYPE_INDEX_SHARED_LOCK(s);
			for (const Handle& h : s)
				hseq.push_back(h);
		}
	}
}

// Same as above, except using an unordered set.
void TypeIndex::get_handles_by_type(UnorderedHandleSet& hset,
                                    Type type,
                                    bool subclass) const
{
	if (not subclass and type < _offset_to_atom) return;

	if (type >= _offset_to_atom)
	{
		int start = get_bucket_start(type);
		for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
		{
			const AtomSet& s(_idx[ibu]);
			TYPE_INDEX_SHARED_LOCK(s);
			hset.insert(s.begin(), s.end());
		}
	}

	// Not subclassing? We are done!
	if (not subclass) return;

	Type tstar = std::max(type+1, _offset_to_atom);
	for (Type t = tstar; t<_num_types; t++)
	{
		if (not _nameserver.isA(t, type)) continue;

		int start = get_bucket_start(t);
		for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
		{
			const AtomSet& s(_idx[ibu]);
			TYPE_INDEX_SHARED_LOCK(s);
			hset.insert(s.begin(), s.end());
		}
	}
}

// ================================================================

void TypeIndex::get_rootset_by_type(HandleSeq& hseq,
                                    Type type,
                                    bool subclass,
                                    const AtomSpace* cas) const
{
	if (not subclass and type < _offset_to_atom) return;

	// Get the initial size of the handles vector.
	size_t initial_size = hseq.size();

	// Determine the number of atoms we'll be adding.
	size_t size_of_append = size(type, subclass);

	// Now reserve size for the addition. This is faster for large
	// append iterations since appends to the list won't require new
	// allocations and copies whenever the allocated size is exceeded.
	hseq.reserve(initial_size + size_of_append);

	if (type >= _offset_to_atom)
	{
		int start = get_bucket_start(type);
		for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
		{
			const AtomSet& s(_idx[ibu]);
			TYPE_INDEX_SHARED_LOCK(s);
			for (const Handle& h : s)
			{
				if (h->isIncomingSetEmpty(cas))
					hseq.push_back(h);
			}
		}
	}

	// Not subclassing? We are done!
	if (not subclass) return;

	Type tstar = std::max(type+1, _offset_to_atom);
	for (Type t = tstar; t<_num_types; t++)
	{
		if (not _nameserver.isA(t, type)) continue;

		int start = get_bucket_start(t);
		for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
		{
			const AtomSet& s(_idx[ibu]);
			TYPE_INDEX_SHARED_LOCK(s);
			for (const Handle& h : s)
				if (h->isIncomingSetEmpty(cas))
					hseq.push_back(h);
		}
	}
}

// ================================================================
