/*
 * VariableList.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
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

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/TypeNode.h>

#include "VariableList.h"

using namespace opencog;

void VariableList::throw_if_not_variable_list(Type t) const
{
	if (not nameserver().isA(t, VARIABLE_LIST))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
		                            "Expecting a VariableList, got %s",
		                            tname.c_str());
	}
}

VariableList::VariableList(const Handle& vardecl)
	: Link(
		not vardecl ?
		// If vardecl is undefined then construct an empty variable list
		HandleSeq({})
		:
		// Otherwise vardecl is either a VariableList, or a naked or
		// typed variable.
		vardecl->get_type() == VARIABLE_LIST ?
		vardecl->getOutgoingSet() : HandleSeq({vardecl}),
		VARIABLE_LIST), _variables(vardecl, true)
{
}

VariableList::VariableList(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t), _variables(_outgoing, true)
{
	throw_if_not_variable_list(t);
}

/* ================================================================= */

std::string opencog::oc_to_string(const VariableListPtr& vlp,
                                  const std::string& indent)
{
	if (vlp == nullptr)
		return indent + "nullvariablelist";
	else
		return oc_to_string(vlp->get_handle(), indent);
}

DEFINE_LINK_FACTORY(VariableList, VARIABLE_LIST)

/* ===================== END OF FILE ===================== */
