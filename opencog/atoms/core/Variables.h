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

#ifndef _OPENCOG_VARIABLES_H
#define _OPENCOG_VARIABLES_H

#include <map>
#include <set>

#include <boost/operators.hpp>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/FreeVariables.h>
#include <opencog/atoms/core/TypedVariableLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef std::map<Handle, TypedVariableLinkPtr> VariableTypeMap;
typedef std::map<Handle, TypeSet> VariableSimpleTypeMap;
typedef std::map<Handle, HandleSet> VariableDeepTypeMap;
typedef std::pair<size_t, size_t> GlobInterval;
typedef std::map<Handle, GlobInterval> GlobIntervalMap;

/// The Variables struct defines a list of typed variables "unbundled"
/// from the hypergraph in which they normally occur. The goal of this
/// structure is to make it easier and faster to work with VariableNodes
/// in C++; specifically, to check any type restrictions that may apply.
///
/// This class is used by VariableList and ScopeLink to define typed,
/// scoped, bound variables; in particular, it is heavily used by the
/// pattern matcher.
///
struct Variables : public FreeVariables,
                   public boost::totally_ordered<Variables>
{
	// CTors. The ordered flag indicates whether we care about the
	// order of the variables. It is false by default and only enabled
	// if VariableList is used.
	Variables(bool ordered=false);
	Variables(const Handle& vardecl, bool ordered=false);
	Variables(const HandleSeq& vardecls, bool ordered=false);

	/// Whether the order matters or not. Typically if constructed with
	/// VariableList then the order matters, if constructed with
	/// VariableSet then the order does not matter.
	bool _ordered;

	/// Unbundled variables and type restrictions for them.

	/// _typemap holds back-ponters to TypedVariableLinkPtrs
	/// _simple_typemap is the (possibly empty) list of restrictions
	/// on the variable types. It holds a disjunction of class Type.
	/// _deep_typemap holds complex or "deep" type definitions, such
	/// as those defined by SignatureLink.
	VariableTypeMap _typemap;
	VariableSimpleTypeMap _simple_typemap;
	VariableDeepTypeMap _deep_typemap;

	/// To restrict how many atoms should be matched for each of the
	/// GlobNodes in the pattern.
	GlobIntervalMap _glob_intervalmap;

	/// Anchor, if present, else undefined.
	Handle _anchor;

	// Validate the variable decls
	void validate_vardecl(const Handle&);
	void validate_vardecl(const HandleSeq&);
	void unpack_vartype(const Handle&);

	/// Return true iff all variables are well typed. For now only
	/// simple types are supported, specifically if some variable is
	/// simple typed NOTYPE, then it returns false.
	bool is_well_typed() const;

	// Return true if the other Variables struct is equal to this one,
	// up to alpha-conversion. That is, same number of variables, same
	// type restrictions, but different actual variable names.
	// Same as satisfying this->is_type(other->varseq) and also
	// other->is_type(this->varseq) -- the equality is symmetric.
	bool is_equal(const Variables& other) const;
	bool is_equal(const Variables& other, size_t index) const;
	bool operator==(const Variables& other) const;

	// Comparison operators. Convenient to define containers of Variables
	bool operator<(const Variables& other) const;

	// Return true if the variable `othervar` in `other` is
	// alpha-convertible to the variable `var` in this. That is,
	// return true if they are the same variable, differing only
	// in name.
	bool is_alpha_convertible(const Handle& var,
	                          const Handle& othervar,
	                          const Variables& other,
	                          bool check_type=false) const;

	// Return true if we are holding a single variable, and the handle
	// given as the argument satisfies the type restrictions (if any).
	// Else return false.
	bool is_type(const Handle&) const;

	// Return true if we are holding a single variable, and it can
	// be the indicated type.
	bool is_type(Type) const;

	// Return true if we are holding the variable `var`, and `val`
	// satisfies the type restrictions that apply to `var`.
	bool is_type(const Handle& var, const Handle& val) const;

	// Return true if the sequence is of the same length as the variable
	// declarations we are holding, and if they satisfy all of the type
	// restrictions (if any).
	bool is_type(const HandleSeq& hseq) const;

	// Return true if the int satisfies the lower bound interval restriction.
	// Return false otherwise.
	bool is_lower_bound(const Handle& glob, size_t n) const;

	// Return true if the int satisfies the upper bound interval restriction.
	// Return false otherwise.
	bool is_upper_bound(const Handle& glob, size_t n) const;

	// Return true if the variable is has a range other than
	// (1,1) i.e. if it can match more than one thing.
	bool is_globby(const Handle& glob) const;

	// Given the tree `tree` containing variables in it, create and
	// return a new tree with the indicated arguments `args` substituted
	// for the variables. The `args` must pass the typecheck, else an
	// exception is thrown. If "silent" is true, then the exception
	// will not be logged; this allows this method to be used for
	// filtering, where type mis-checks are expected and normal.
	Handle substitute(const Handle& tree,
	                  const HandleSeq& args,
	                  bool silent=false) const;

	// Like the above, but using a partial map.
	Handle substitute(const Handle& tree,
	                  const HandleMap& map,
	                  bool silent=false) const;

	// Extend this by adding in the given variables. If either this or
	// the other are ordered, then the result is ordered
	void extend(const Variables&);

	// Erase the given variable, if exist
	void erase(const Handle&);

	/// Return the TypedVariableLink for the indicated variable.
	/// Return just the Variable itself, if its not typed.
	Handle get_type_decl(const Handle&, const Handle&) const;

	/// Inverse of Variables(vardecl).get_variable()
	///
	/// That is, convert Variables object into a variable declaration,
	/// that is a VariableList, VariableSet, TypedVariableLink,
	/// VariableNode or GlobNode, suitable for direct use in a
	/// ScopeLink.
	///
	/// If empty then return the empty VariableList or VariableSet.
	Handle get_vardecl() const;

	/// Like FreeVariables::find_variables but set _ordered to false,
	/// on the ground that if this method is called, then no order
	/// was intended in the variable scope.
	void find_variables(const Handle& body);
	void find_variables(const HandleSeq& oset, bool ordered_link=true);

	const GlobInterval& get_interval(const Handle&) const;

	// Useful for debugging
	std::string to_string(const std::string& indent=empty_string) const;

protected:

	void extend_interval(const Handle &h, const Variables &vset);
};

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const VariableSimpleTypeMap& vtm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const GlobIntervalMap& gim,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Variables& var,
                         const std::string& indent=empty_string);

/** @}*/
}

#endif // _OPENCOG_VARIABLES_H
