/*
 * DeleteLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/AtomSpace.h>

#include "DeleteLink.h"

using namespace opencog;

void DeleteLink::init(void)
{
	FreeLink::init();
}

void DeleteLink::setAtomSpace(AtomSpace * as)
{
	// The handleset must contain a variable in it, somewhere.
	// If it doesn't, then the entire handleset is to be deleted
	// (removed from the atomspace).
	if (0 <= _vars.varseq.size())
	{
		Atom::setAtomSpace(as);
		return;
	}

	for (const Handle& h : _outgoing)
		as->extract_atom(h, true);

	// The AtomSpace code seems to want this exception, so that
	// the atom gets deleted from the backingstore too.  But we could
	// just as easily call `as->delete_atom()` above!?
	// throw DeleteException();
}

#if 0
/*****
Hmm. This seems not to be needed, right now.
****/
Handle DeleteLink::execute(AtomSpace * as) const
{
	const HandleSeq& oset = _outgoing;
	for (const Handle& h : oset)
	{
		Type t = h->get_type();
		if (VARIABLE_NODE != t)
			as->removeAtom(h, true);
	}
	return Handle::UNDEFINED;
}
#endif

DeleteLink::DeleteLink(const HandleSeq&& oset, Type type)
	: FreeLink(std::move(oset), type)
{
	init();
}

DEFINE_LINK_FACTORY(DeleteLink, DELETE_LINK)

/* ===================== END OF FILE ===================== */
