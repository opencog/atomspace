/*
 * opencog/persist/api/BackingStore.cc
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/BackingStore.h>

using namespace opencog;

void BackingStore::registerWith(AtomSpace* atomspace)
{
	atomspace->registerBackingStore(this);
}

void BackingStore::unregisterWith(AtomSpace* atomspace)
{
	atomspace->unregisterBackingStore(this);
}

/* Provide a backwards-compat implementation. */
void BackingStore::getAtom(const Handle& h)
{
	Handle hv;
	if (h->is_node())
		hv = getNode(h->get_type(), h->get_name().c_str());
	else
		hv = getLink(h->get_type(), h->getOutgoingSet());

	barrier();
	if (hv)
	{
		AtomSpace *as = h->getAtomSpace();
		if (nullptr != as)
			for (const Handle& k: hv->getKeys())
			{
				Handle ak = as->add_atom(k);
				// Read-only AtomSpaces won't allow insertion.
				if (nullptr == ak) continue;
				as->set_value(h, ak, hv->getValue(k));
			}
		else
			h->copyValues(hv);
	}
}
