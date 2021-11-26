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
	_nameserver(nameserver())
{
	resize();
}

void TypeIndex::resize(void)
{
	_num_types = nameserver().getNumberOfClasses();
	TYPE_INDEX_LOCK;
	_idx.resize(_num_types + 1);
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

	TYPE_INDEX_LOCK;
	const AtomSet& s(_idx.at(type));
	for (const Handle& h : s)
		hseq.push_back(h);

	// Not subclassing? We are done!
	if (not subclass) return;

	for (Type t = ATOM; t<_num_types; t++)
	{
		if (t == type or not _nameserver.isA(t, type)) continue;

		const AtomSet& s(_idx.at(t));
		for (const Handle& h : s)
			hseq.push_back(h);
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

	TYPE_INDEX_LOCK;
	const AtomSet& s(_idx.at(type));
	for (const Handle& h : s)
	{
		if (h->isIncomingSetEmpty(cas))
			hseq.push_back(h);
	}

	// Not subclassing? We are done!
	if (not subclass) return;

	for (Type t = ATOM; t<_num_types; t++)
	{
		if (t == type or not _nameserver.isA(t, type)) continue;

		const AtomSet& s(_idx.at(t));
		for (const Handle& h : s)
			if (h->isIncomingSetEmpty(cas))
				hseq.push_back(h);
	}
}

// ================================================================
