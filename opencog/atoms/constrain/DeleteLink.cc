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

#include <opencog/atoms/free/FindUtils.h>
#include <opencog/atoms/value/VoidValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "DeleteLink.h"

using namespace opencog;

void DeleteLink::setAtomSpace(AtomSpace * as)
{
	// The handleset must contain a variable in it, somewhere.
	// If it doesn't, then the entire handleset is to be deleted
	// (removed from the atomspace).
	if (not is_closed(get_handle()))
	{
		Atom::setAtomSpace(as);
		return;
	}

	DeleteLink::execute(as, true);

	throw DeleteException();
}

ValuePtr DeleteLink::execute(AtomSpace * as, bool silent)
{
	// Self-delete only when fully-grounded. Do nothing, if there
	// are variables. The goal is to allow DeleteLinks to be used in
	// query patterns (where they will have ... variables in them!)
	if (not is_closed(get_handle()))
		return createVoidValue();

	// In general, neither this link, nor it's outgoing set will be in
	// any AtomSpace at all. So, in order for the delete to be successful,
	// an AtomSpace to delete from must be explicitly specified. The
	// reason the outgoing set is not in any AtomSpace is because this
	// DeleteLink got assembled on the fly, usually by a PutLink, and
	// so of course ... it's not anywhere, yet.
	if (nullptr == as)
		throw InvalidParamException(TRACE_INFO,
			"DeleteLink::execute() expects AtomSpace");

	if (DELETE_RECURSIVE_LINK == _type)
	{
		for (const Handle& h : _outgoing)
		{
			AtomSpace* oas = h->getAtomSpace();
			if (nullptr == oas) oas = as;
			oas->extract_atom(h, true);
		}
	}
	else
	{
		for (const Handle& h : _outgoing)
		{
			AtomSpace* oas = h->getAtomSpace();
			if (nullptr == oas) oas = as;
			oas->extract_atom(h, false);
		}
	}

	return createVoidValue();
}

DeleteLink::DeleteLink(const HandleSeq&& oset, Type type)
	: Link(std::move(oset), type)
{
}

DEFINE_LINK_FACTORY(DeleteLink, DELETE_LINK)

/* ===================== END OF FILE ===================== */
