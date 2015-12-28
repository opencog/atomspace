/*
 * opencog/atoms/core/Variables.h
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

#ifndef _OPENCOG_VARIABLE_H
#define _OPENCOG_VARIABLE_H

#include <map>
#include <set>

#include <opencog/atomspace/Handle.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The FreeVariables struct defines a list of free, untyped variables
/// "unbundled" from the hypergraph in which they normally occur. The
/// goal of this structure is to make it easier and faster to work with
/// VariableNodes in C++; specifically, to find thier locations within
/// a hypergraph, and to perform beta-substitution (to substitute a
/// value for the variable).  This class implements the data that is
/// used by FreeLink to work with free variables.
///
struct FreeVariables
{
	/// Unbundled variables (i.e. pulled out of the graph they live in).
	///
	/// The varset contains exactly the same atoms as the varseq; it
	/// is used for fast lookup; (i.e. is some some variable a part of
	/// this set?) whereas the varseq list preserves the original order
	/// of the variables.  Yes, the fast lookup really is needed!
	///
	/// The index is a reversed index into varseq: given a variable,
	/// it returns the ordinal of that variable in the varseq. It is
	/// used to implement the variable substitution (aka beta-reducation
	/// aka "PutLink") method.
	HandleSeq varseq;
	std::set<Handle> varset;
	std::map<Handle, unsigned int> index;

	/// Create an ordered set of the free variables in the given oset.
	///
	/// By "ordered set" it is meant: a list of variables, in traversal
	/// order (from left to right, as they appear in the tree), with each
	/// variable being named only once.  The varset is only used to make
	/// sure that we don't name a variable more than once; that's all.
	///
	/// Variables that are inside a QuoteLink are ignored ... unless they
	/// are wrapped by UnquoteLink.  That is, QuoteLink behaves like a
	/// quasi-quote in lisp/scheme.
	///
	/// Variables that are bound inside of some deeper link are ignored;
	/// they are not free, and thus must not be collected up.  That is,
	/// any bound variables appearing in a GetLink, BindLink,
	/// SatisfactionLink, etc. will not be collected.  Any *free* variables
	/// in these same links *will* be collected (since they are free!)
	void find_variables(const Handle&);
	void find_variables(const HandleSeq&);

	// Given the tree `tree` containing variables in it, create and
	// return a new tree with the indicated values `vals` substituted
	// for the variables.  "nocheck" == no type checking is done.
	Handle substitute_nocheck(const Handle&, const HandleSeq&) const;
};

typedef std::map<Handle, const std::set<Type> > VariableTypeMap;
typedef std::map<Handle, const std::set<Handle> > VariableDeepTypeMap;

/// The Variables struct defines a list of typed variables "unbundled"
/// from the hypergraph in which they normally occur. The goal of this
/// structure is to make it easier and faster to work with VariableNodes
/// in C++; specifically, to check any type restrictions that may apply.
///
/// This class is used by VariableList and ScopeLink to define typed,
/// scoped, bound variables; in particular, it is heavily used by the
/// pattern matcher.
///
struct Variables : public FreeVariables
{
	/// Unbundled variables and type restrictions for them.

	/// _simple_typemap is the (possibly empty) list of restrictions
	/// on the variable types. It holds a disjunction of class Type.
	/// _deep_typemap holds complex or "deep" type definitions, such
	/// as those defined by SignatureLink.
	/// _fuzzy_typemap holds approximate of "fuzzy" type definitions,
	/// those which only need to be approximately matched.
	VariableTypeMap _simple_typemap;
	VariableDeepTypeMap _deep_typemap;
	VariableDeepTypeMap _fuzzy_typemap;

	// Return true if we are holding a single variable, and the handle
	// given as the argument satisfies the type restrictions (if any).
	// Else return false.
	bool is_type(const Handle& h) const;

	// Return true if we are holding the variable `var`, and `val`
	// satisfies the type restrictions that apply to `var`.
	bool is_type(const Handle& var, const Handle& val) const;

	// Return true if the sequence is of the same length as the variable
	// declarations we are holding, and if they satisfy all of the type
	// restrictions (if any).
	bool is_type(const HandleSeq& hseq) const;

	// Given the tree `tree` containing variables in it, create and
	// return a new tree with the indicated values `vals` substituted
	// for the variables. The vals must pass the typecheck, else an
	// exception is thrown. An exception is thrown if the vals are not
	// of the types specified in this class.
	Handle substitute(const Handle& tree, const HandleSeq& vals) const;

	// Extend this variable set by adding in the given variable set.
	void extend(const Variables&);

	// Useful for debugging
	std::string to_string() const;

private:
	bool is_type_rec(Handle, const Handle&) const;
};

/** @}*/
}

#endif // _OPENCOG_VARIABLE_H
