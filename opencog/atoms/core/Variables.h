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

#include <boost/operators.hpp>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/Replacement.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

// Struct to build paths between variables and root. A path is a
// sequence of pairs (Type, Index), where Type is the type of a link
// and index is the index of the outgoing of that link. If the type
// unordered however, then index is zero, because in such case the
// index has no meaning. The collection of paths is stored in a
// multiset because some paths can be identically looking while in
// fact different in reality, due to the index of a unordered link
// being always zero. We do that to be able to use std::operator<
// rather than provide our own.
//
// Note: a notion of path is already implemented in PatternTerm,
// It might be a good idea to unity this mechanism with that one.
typedef std::pair<Type, Arity> TypeArityPair;
typedef std::vector<TypeArityPair> Path;
typedef std::multiset<Path> PathMultiset;
typedef std::map<Handle, PathMultiset> HandlePathsMap;

/// The FreeVariables struct defines a list of free, untyped variables
/// "unbundled" from the hypergraph in which they normally occur. The
/// goal of this structure is to make it easier and faster to work with
/// VariableNodes in C++; specifically, to find thier locations within
/// a hypergraph, and to perform beta-substitution (to substitute a
/// argument for the variable).  This class implements the data that is
/// used by FreeLink to work with free variables.
///
struct FreeVariables : Replacement
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
	/// used to implement the variable substitution (aka beta-reduction
	/// aka "PutLink") method.
	HandleSeq varseq;
	HandleSet varset;
	IndexMap index;

	// CTor, mostly convenient for unit tests
	FreeVariables() {}
	FreeVariables(const std::initializer_list<Handle>& variables);

	// Construct index. FreeVariables::varseq must be previously
	// defined.
	void init_index();

	/// Return true if the variables in this, and other, are the same
	/// variables (have exactly the same variable names.)
	bool is_identical(const FreeVariables& other) const;

	/// Return true if variable `var` is in this variableset.
	bool is_in_varset(const Handle& v) const;

	/// Return true if all variables within the given range is in
	/// varset.
	template<typename It>
	bool are_in_varset(It from, It to) const {
		return std::all_of(from, to, [&](const Handle& v)
		                   { return is_in_varset(v); });
	}
	template <typename C>
	bool are_in_varset(const C& c) const {
		return are_in_varset(c.begin(), c.end());
	}

	/// Create an ordered set of the free variables in the given body.
	///
	/// By "ordered set" it is meant: a list of variables in a
	/// cannonical order that is compatible with alpha-equivalence. So
	/// that if variables are renamed the order will not be altered in
	/// a way that modifies the semantics of the scope.
	///
	/// Quotations and scopes are supported (so quoted or scoped
	/// variables are not considered).
	void find_variables(const Handle& body);

	/// Like above but for outgoing sets. The link_ordered flag
	/// indicates whether the outgoing set is associated with an
	/// ordered link.
	void find_variables(const HandleSeq& oset, bool ordered_link=true);

	/// Sort the variables in a canonical order determined by their
	/// positions in the given outgoing set, which is assumed ordered,
	/// as outgoing sets of scopes are always ordered so far.  In
	/// ordered link, the ordered is determined by the outgoing set
	/// order (from left to right).  In unordered links, the ordered is
	/// determined by some arbitrary, though semantically consistent
	/// fix order.  The order only depends on variable names as last
	/// resort, when no semantic property can be used to break the
	/// symmetry.
	void canonical_sort(const HandleSeq& outgoings);

	/// Convert a variable->argument mapping into a sequence of
	/// "arguments" that are in the same order as the free variables
	/// in this class.  If the mapping does not mention a variable,
	/// then that variable itself is used as the argument.  This sequence
	/// can be used with the substitute_nocheck() function below.
	HandleSeq make_sequence(const HandleMap&) const;

	/// Erase the given variable, if it exists.
	void erase(const Handle&);

	/// Given the tree `tree` containing variables in it, create and
	/// return a new tree with the indicated arguments `args` substituted
	/// for the variables.  "nocheck" == no type checking is done.
	/// This performs an almost pure, syntactic beta-reduction; its
	/// almost-pure because it does honour the semantics of QuoteLink.
	Handle substitute_nocheck(const Handle&,
	                          const HandleSeq&,
	                          bool silent=false) const;

	/// Like the above, but takes a mapping from variables to arguments.
	Handle substitute_nocheck(const Handle&,
	                          const HandleMap&,
	                          bool silent=false) const;

	/// Comparison operator. Used to enable containers holding
	/// this class.
	bool operator<(const FreeVariables& other) const;

	/// Return the number of free variables that this class is holding.
	std::size_t size() const;

	/// Return true, if there are no free variables.
	bool empty() const;

	/// Useful for debugging
	std::string to_string(const std::string& indent=empty_string) const;
};

typedef std::map<Handle, TypeSet> VariableTypeMap;
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

	/// _simple_typemap is the (possibly empty) list of restrictions
	/// on the variable types. It holds a disjunction of class Type.
	/// _deep_typemap holds complex or "deep" type definitions, such
	/// as those defined by SignatureLink.
	/// _fuzzy_typemap holds approximate of "fuzzy" type definitions,
	/// those which only need to be approximately matched.
	VariableTypeMap _simple_typemap;
	VariableDeepTypeMap _deep_typemap;
	VariableDeepTypeMap _fuzzy_typemap;

	/// To restrict how many atoms should be matched for each of the
	/// GlobNodes in the pattern.
	GlobIntervalMap _glob_intervalmap;

	/// Anchor, if present, else undefined.
	Handle _anchor;

	// See VariableList.cc for comments
	void get_vartype(const Handle&);

	// Validate the variable decls
	void validate_vardecl(const Handle&);
	void validate_vardecl(const HandleSeq&);

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

	// Return true if the it satisfies the lower bound interval restriction.
	// Return false otherwise.
	bool is_lower_bound(const Handle& glob, size_t n) const;

	// Return true if the it satisfies the upper bound interval restriction.
	// Return false otherwise.
	bool is_upper_bound(const Handle& glob, size_t n) const;

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
	///
	/// TODO: support deep and fuzzy typemaps.
	Handle get_vardecl() const;

	/// Like FreeVariables::find_variables but set _ordered to false,
	/// on the ground that if such a method is called then no ordered
	/// was provided by the creator of that scope, and thus order is
	/// not relevant.
	void find_variables(const Handle& body);
	void find_variables(const HandleSeq& oset, bool ordered_link=true);

	const GlobInterval& get_interval(const Handle&) const;

	// Useful for debugging
	std::string to_string(const std::string& indent=empty_string) const;

protected:
	bool is_type(VariableTypeMap::const_iterator,
			VariableDeepTypeMap::const_iterator,
			VariableDeepTypeMap::const_iterator,
			const Handle&) const;

	void extend_interval(const Handle &h, const Variables &vset);
};

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
struct VarScraper;
std::string oc_to_string(const TypeArityPair& tap,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Path& path,
                         const std::string& indent=empty_string);
std::string oc_to_string(const PathMultiset& paths,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandlePathsMap& hpsm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const VarScraper& vsc,
                         const std::string& indent=empty_string);
std::string oc_to_string(const FreeVariables::IndexMap& imap,
                         const std::string& indent=empty_string);
std::string oc_to_string(const VariableTypeMap& vtm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const GlobIntervalMap& gim,
                         const std::string& indent=empty_string);
std::string oc_to_string(const FreeVariables& var,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Variables& var,
                         const std::string& indent=empty_string);

/** @}*/
}

#endif // _OPENCOG_VARIABLE_H
