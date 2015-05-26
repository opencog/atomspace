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
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomutils/FindUtils.h>

#include "DeleteLink.h"

using namespace opencog;

void DeleteLink::init(void)
{
	// The handleset must contain a free variable in it, somewhere.
	// If it doesn't, then the entire Handleset should be deleted
	// (removed from the atomspace). We can't do this at constructor
	// time, because we don't know the atomspace yet.  So we hack
	// around this by thowing at construtor time.
	//
	if (0 == _varseq.size())
		// throw DeleteException();
		throw InvalidParamException(TRACE_INFO,
			"Cannot create a fully grounded DeleteLink!");
} 

#if 0
/*****
Well, we cannot really implement this here; but this is what
it should actually do.  We can't implement it here, because
fully-grounded DeleteLink's cannot exist.
****/
Handle DeleteLink::execute(AtomSpace * as) const
{
	const HandleSeq& oset = _outgoing;
	for (const Handle& h : oset)
	{
		Type t = h->getType();
		if (VARIABLE_NODE != t)
			as->removeAtom(h, true);
	}
	return Handle::UNDEFINED;
}
#endif

DeleteLink::DeleteLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(DELETE_LINK, oset, tv, av)
{
	init();
}

DeleteLink::DeleteLink(Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, DELETE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a DeleteLink, got %s", tname.c_str());
	}

	init();
}

/* ===================== END OF FILE ===================== */
