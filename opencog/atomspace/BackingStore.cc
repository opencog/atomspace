/*
 * opencog/atomspace/BackingStore.cc
 *
 * Copyright (C) 2013 Linas Vepstas
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

#include <algorithm>

#include "BackingStore.h"

#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

#define DEBUG_IGNORE 0

#if DEBUG_IGNORE
std::string s_indent("");
int s_ignore_count = 0;
#endif

bool BackingStore::ignoreAtom(const Handle& h) const
{
#if DEBUG_IGNORE
	fprintf(stderr, "%signoreAtom(%lu)\n", s_indent.c_str(),
	        s_ignore_count++);
#endif

	// If the handle is a uuid only, and no atom, we can't ignore it.
	if (nullptr == h) return false;

	// If the atom is of an ignoreable type, then ignore.
	if (ignoreType(h->get_type())) return true;

	// If its a link, then scan the outgoing set.
	if (not h->is_link()) return false;

#if DEBUG_IGNORE
	s_indent += "  ";
#endif
	bool should_ignore = false;
	const HandleSeq& hs = h->getOutgoingSet();
	if (std::any_of(hs.begin(), hs.end(), [this](Handle ho) { return ignoreAtom(ho); }))
		should_ignore = true;
#if DEBUG_IGNORE
	s_indent.resize(max(s_indent.size() - 2, 0));
#endif

	return should_ignore;
}

void BackingStore::registerWith(AtomSpace* atomspace)
{
	atomspace->registerBackingStore(this);
}

void BackingStore::unregisterWith(AtomSpace* atomspace)
{
	atomspace->unregisterBackingStore(this);
}

