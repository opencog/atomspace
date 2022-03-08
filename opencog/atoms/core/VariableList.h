/*
 * opencog/atoms/core/VariableList.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_VARIABLE_LIST_H
#define _OPENCOG_VARIABLE_LIST_H

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/Variables.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The VariableList class records it's outgoing set in various ways
/// that make it easier and faster to work with in C++.  It implements
/// a substitute method that will replace all variables in a tree by
/// the corresponding atoms that it is given. See the .cc file for
/// more info.
///
/// The constructors make sure that the contents of the variable list
/// are syntactically correct; i.e that it actually contains variables.
/// Otherwise, it throws an error on bad syntax.  Thus, bad
/// VariableLists cannot be inserted into the atomspace.
class VariableList : public Link
{
protected:
	/// Unbundled variables and types for them.
	Variables _variables;

	VariableList(Type, const HandleSeq&);

	void throw_if_not_variable_list(Type t) const;
public:
	VariableList(const HandleSeq&& vardecls, Type=VARIABLE_LIST);
	VariableList(const Handle& hvardecls);

	VariableList(const VariableList&) = delete;
	VariableList& operator=(const VariableList&) = delete;

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _variables; }

	// Return true if we are holding a single variable, and the handle
	// given as the argument satisfies the type restrictions (if any).
	// Else return false.
	bool is_type(const Handle& h) const { return _variables.is_type(h); }

	// Return true if we are holding the variable `var`, and `val`
	// satisfies the type restrictions that apply to `var`.
	bool is_type(const Handle& var, const Handle& val) const
		{ return _variables.is_type(var, val); }

	// Return true if the sequence is of the same length as the variable
	// declarations we are holding, and if they satisfy all of the type
	// restrictions (if any).
	bool is_type(const HandleSeq& hseq) const { return _variables.is_type(hseq); }

	// Given the tree `tree` containing variables in it, create and
	// return a new tree with the indicated arguments `args` substituted
	// for the variables. The `args` must pass the typecheck, else an
	// exception is thrown.
	Handle substitute(const Handle& tree, const HandleSeq& args) const
		{ return _variables.substitute(tree, args); }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(VariableList)
#define createVariableList CREATE_DECL(VariableList)

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const VariableListPtr& vlp,
                         const std::string& indent=empty_string);

/** @}*/
}

#endif // _OPENCOG_VARIABLE_LIST_H
