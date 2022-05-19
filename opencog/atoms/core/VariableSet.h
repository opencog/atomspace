/*
 * opencog/atoms/core/VariableSet.h
 *
 * Copyright (C) 2019 SingularityNET Foundation
 * All Rights Reserved
 *
 * Author: Nil Geisweiller <ngeiswei@gmail.com>
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

#ifndef _OPENCOG_VARIABLE_SET_H
#define _OPENCOG_VARIABLE_SET_H

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/core/UnorderedLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class VariableSet : public UnorderedLink
{
protected:
	/// Unbundled variables and types for them.
	Variables _variables;

	void throw_if_not_variable_set(Type t) const;
	
public:
	VariableSet(const HandleSeq&& vardecls, Type=VARIABLE_SET);
	VariableSet(const Handle& hvardecls);
	VariableSet(const VariableSet&) = delete;
	VariableSet& operator=(const VariableSet&) = delete;

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _variables; }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(VariableSet)
#define createVariableSet CREATE_DECL(VariableSet)

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const VariableSetPtr& vsp,
                         const std::string& indent=empty_string);

/** @}*/
}

#endif // _OPENCOG_VARIABLE_SET_H
