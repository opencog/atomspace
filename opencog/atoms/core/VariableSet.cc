/*
 * VariableSet.cc
 *
 * Copyright (C) 2019 SingularityNET Foundation
 * All Rights Reserved
 *
 * Author: Nil Geisweiller <ngeiswei@gmail.com>
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

#include "VariableSet.h"

#include <opencog/atoms/base/ClassServer.h>

using namespace opencog;

void VariableSet::throw_if_not_variable_set(Type t) const
{
	if (not nameserver().isA(t, VARIABLE_SET))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a VariableSet, got %s", tname.c_str());
	}
}

VariableSet::VariableSet(const HandleSeq&& vardecls, Type t)
	: UnorderedLink(std::move(vardecls), t), _variables(_outgoing, false)
{
	throw_if_not_variable_set(t);
}

VariableSet::VariableSet(const Handle& vardecl)
	: UnorderedLink(
		not vardecl ?
		// If vardecl is undefined then construct an empty variable set
		HandleSeq({})
		:
		// Otherwise vardecl is either a VariableSet, or a naked or
		// typed variable.
		vardecl->get_type() == VARIABLE_SET ?
		vardecl->getOutgoingSet() : HandleSeq({vardecl}),
		VARIABLE_SET), _variables(vardecl)
{
}

std::string opencog::oc_to_string(const VariableSetPtr& vsp,
                                  const std::string& indent)
{
	if (vsp == nullptr)
		return indent + "nullvariableset";
	else
		return oc_to_string(vsp->get_handle(), indent);
}

DEFINE_LINK_FACTORY(VariableSet, VARIABLE_SET)
