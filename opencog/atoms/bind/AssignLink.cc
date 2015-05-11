/*
 * AssignLink.cc
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

#include "AssignLink.h"

using namespace opencog;

void AssignLink::init(const HandleSeq& oset)
{
	// The first member of the handleset must be a TypeNode, and it must
	// name a valid atom type.

	if (0 == oset.size())
		throw InvalidParamException(TRACE_INFO,
			"AssignLinks must have members!");

	Type t = oset[0]->getType();
	if (TYPE_NODE != t)
		throw InvalidParamException(TRACE_INFO,
			"Invalid format for a AssignLink! First member must be a type node!");

	const std::string& name = NodeCast(oset[0])->getName();
	if (not classserver().isDefined(name))
		throw InvalidParamException(TRACE_INFO,
			"Invalid format for a AssignLink! Not a defined type!");

	// Cache the type and the oset
	_link_type = classserver().getType(name);

	for (size_t j=1; j< oset.size(); j++)
		_outset.push_back(oset[j]);
} 

Handle AssignLink::execute(AtomSpace * as) const
{
	return createLink(_link_type, _outset);
}

AssignLink::AssignLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(ASSIGN_LINK, oset, tv, av)
{
	init(oset);
}

AssignLink::AssignLink(Type t, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, oset, tv, av)
{
	if (not classserver().isA(t, ASSIGN_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an AssignLink, got %s", tname.c_str());
	}

	init(oset);
}

AssignLink::AssignLink(Link &l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, ASSIGN_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an AssignLink, got %s", tname.c_str());
	}

	init(l.getOutgoingSet());
}

// ============================================================

Handle AddLink::execute(AtomSpace* as) const
{
	if (NULL == as)
		return createLink(_link_type, _outset);
	return as->addAtom(createLink(_link_type, _outset));
}

AddLink::AddLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: AssignLink(ADD_LINK, oset, tv, av)
{
	init(oset);
}

AddLink::AddLink(Link &l)
	: AssignLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, ADD_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a AddLink, got %s", tname.c_str());
	}

	init(l.getOutgoingSet());
}

// ============================================================

Handler RemoveLink::execute(AtomSpace* as) const
{
	// Are there *any* constants in the outgoing set?
	int narrowest = -1;
	size_t sz = SIZE_T_MAX;
	for (int i=0; i< _outset.size(); i++)
	{
	}

	as->getHandlesByType(seq, _link_type)
	return createLink(_link_type, _outset);
}

RemoveLink::RemoveLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: AssignLink(REMOVE_LINK, oset, tv, av)
{
	init(oset);
}

RemoveLink::RemoveLink(Link &l)
	: AssignLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, REMOVE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a RemoveLink, got %s", tname.c_str());
	}

	init(l.getOutgoingSet());
}

/* ===================== END OF FILE ===================== */
