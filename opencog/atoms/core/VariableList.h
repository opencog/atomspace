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
	Variables _varlist;

	// See VariableList.cc for comments
	void get_vartype(const Handle&);

	// Validate the variable decls
	void validate_vardecl(const Handle&);
	void validate_vardecl(const HandleSeq&);

	VariableList(Type, const HandleSeq&);

	void build_index(void);
public:
	VariableList(const HandleSeq& vardecls, Type=VARIABLE_LIST);
	VariableList(const Handle& hvardecls);
	VariableList(const Link&);

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _varlist; }

	// Return true if we are holding a single variable, and the handle
	// given as the argument satisfies the type restrictions (if any).
	// Else return false.
	bool is_type(const Handle& h) const { return _varlist.is_type(h); }

	// Return true if we are holding the variable `var`, and `val`
	// satisfies the type restrictions that apply to `var`.
	bool is_type(const Handle& var, const Handle& val) const
		{ return _varlist.is_type(var, val); }

	// Return true if the sequence is of the same length as the variable
	// declarations we are holding, and if they satisfy all of the type
	// restrictions (if any).
	bool is_type(const HandleSeq& hseq) const { return _varlist.is_type(hseq); }

	// Given the tree `tree` containing variables in it, create and
	// return a new tree with the indicated values `vals` substituted
	// for the variables. The vals must pass the typecheck, else an
	// exception is thrown.
	Handle substitute(const Handle& tree, const HandleSeq& vals) const
		{ return _varlist.substitute(tree, vals); }

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<VariableList> VariableListPtr;
static inline VariableListPtr VariableListCast(const Handle& h)
	{ return std::dynamic_pointer_cast<VariableList>(AtomCast(h)); }
static inline VariableListPtr VariableListCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<VariableList>(a); }

// XXX temporary hack ...
#define createVariableList std::make_shared<VariableList>

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const VariableListPtr& vlp);

/** @}*/
}

#endif // _OPENCOG_VARIABLE_LIST_H
