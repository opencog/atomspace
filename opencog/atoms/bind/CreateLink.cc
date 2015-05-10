/*
 * CreateLink.cc
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

#include "CreateLink.h"

using namespace opencog;

void CreateLink::init(const HandleSeq& oset)
{
	// The first member of the handleset must be a TypeNode, and it must
	// name a valid atom type.

	if (0 == oset.size())
		throw InvalidParamException(TRACE_INFO,
			"CreateLinks must have members!");

	Type t = oset[0]->getType();
	if (TYPE_NODE != t)
		throw InvalidParamException(TRACE_INFO,
			"Invalid format for a CreateLink! First member must be a type node!");

	const std::string& name = NodeCast(oset[0])->getName();
	if (not classserver().isDefined(name))
		throw InvalidParamException(TRACE_INFO,
			"Invalid format for a CreateLink! Not a defined type!");

	// Cache the type and the oset
	_link_type = classserver().getType(name);

	for (size_t j=1; j< oset.size(); j++)
		_outset.push_back(oset[j]);
} 

LinkPtr CreateLink::create(void) const
{
	return createLink(_link_type, _outset);
}

CreateLink::CreateLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(CREATE_LINK, oset, tv, av)
{
	init(oset);
}

CreateLink::CreateLink(const Handle& name, const Handle& defn,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(CREATE_LINK, HandleSeq({name, defn}), tv, av)
{
	init(getOutgoingSet());
}

CreateLink::CreateLink(Link &l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, CREATE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a CreateLink, got %s", tname.c_str());
	}

	init(l.getOutgoingSet());
}

/* ===================== END OF FILE ===================== */
