/*
 * opencog/atomspace/TypeIndex.cc
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

#include "TypeIndex.h"
#include <opencog/atoms/atom_types/NameServer.h>

using namespace opencog;

TypeIndex::TypeIndex(void) :
	_num_types(MAX_SUPPORTED_TYPES),
	_nameserver(nameserver()),
	_idx(POOL_SIZE * _num_types + 1)
{
	resize();
}

void TypeIndex::resize(void)
{
	size_t newsz = nameserver().getNumberOfClasses();
	if (_num_types <= newsz)
		throw RuntimeException(TRACE_INFO,
			"Ran out of space for new types!");
}

void TypeIndex::clear(void)
{
	std::vector<AtomSet> dead;
	{
		// std::shared_lock<std::shared_mutex> lck(_idxmtx);
		dead.swap(_idx);

		// Clear the AtomSpace before releasing the lock.
		for (auto& s : dead)
		{
			TYPE_INDEX_UNIQUE_LOCK(s);
			for (auto& h : s)
				h->_atom_space = nullptr;
		}
	}

	// Do the final cleanup after releasing the lock. This enables
	// the very unlikely situation of having other threads start
	// using the AtomSpace again, while we do final cleanup. BTW,
	// the matching `install()` for the `remove()` below happened
	// in the `AtomSpace::add()` method. We do it here cause its
	// easier. Anyway, we can't do the `remove()` under the lock,
	// that would result in lock inversion.
	for (auto& s : dead)
	{
		TYPE_INDEX_UNIQUE_LOCK(s);
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
	// Get the initial size of the handles vector.
	size_t initial_size = hseq.size();

	// Determine the number of atoms we'll be adding.
	size_t size_of_append = size(type, subclass);

	// Now reserve size for the addition. This is faster for large
	// append iterations since appends to the list won't require new
	// allocations and copies whenever the allocated size is exceeded.
	hseq.reserve(initial_size + size_of_append);

	int start = get_bucket_start(type);
	for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
	{
		const AtomSet& s(_idx.at(ibu));
		TYPE_INDEX_SHARED_LOCK(s);
		for (const Handle& h : s)
			hseq.push_back(h);
	}

	// Not subclassing? We are done!
	if (not subclass) return;

	for (Type t = type+1; t<_num_types; t++)
	{
		if (not _nameserver.isA(t, type)) continue;

		int start = get_bucket_start(type);
		for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
		{
			const AtomSet& s(_idx.at(ibu));
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
	int start = get_bucket_start(type);
	for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
	{
		const AtomSet& s(_idx.at(ibu));
		TYPE_INDEX_SHARED_LOCK(s);
		hset.insert(s.begin(), s.end());
	}

	// Not subclassing? We are done!
	if (not subclass) return;

	for (Type t = type+1; t<_num_types; t++)
	{
		if (not _nameserver.isA(t, type)) continue;

		int start = get_bucket_start(type);
		for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
		{
			const AtomSet& s(_idx.at(ibu));
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
	// Get the initial size of the handles vector.
	size_t initial_size = hseq.size();

	// Determine the number of atoms we'll be adding.
	size_t size_of_append = size(type, subclass);

	// Now reserve size for the addition. This is faster for large
	// append iterations since appends to the list won't require new
	// allocations and copies whenever the allocated size is exceeded.
	hseq.reserve(initial_size + size_of_append);

	const AtomSet& s(_idx.at(type));
	{
		TYPE_INDEX_SHARED_LOCK(s);
		for (const Handle& h : s)
		{
			if (h->isIncomingSetEmpty(cas))
				hseq.push_back(h);
		}
	}

	// Not subclassing? We are done!
	if (not subclass) return;

	for (Type t = type+1; t<_num_types; t++)
	{
		if (not _nameserver.isA(t, type)) continue;

		const AtomSet& s(_idx.at(t));
		TYPE_INDEX_SHARED_LOCK(s);
		for (const Handle& h : s)
			if (h->isIncomingSetEmpty(cas))
				hseq.push_back(h);
	}
}

// ================================================================
