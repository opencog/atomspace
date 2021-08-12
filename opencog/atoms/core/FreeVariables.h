/*
 * opencog/atoms/core/FreeVariables.h
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

#ifndef _OPENCOG_FREE_VARIABLES_H
#define _OPENCOG_FREE_VARIABLES_H

#include <map>
#include <set>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/Replacement.h>

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
	bool varset_contains(const Handle& var) const;

	/// Return true if all variables within the given range is in
	/// varset.
	template<typename It>
	bool varset_includes(It from, It to) const {
		return std::all_of(from, to, [&](const Handle& v)
		                   { return varset_contains(v); });
	}
	template <typename C>
	bool varset_includes(const C& c) const {
		return varset_includes(c.begin(), c.end());
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

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
struct VarScraper;
typedef std::pair<Type, Arity> TypeArityPair;
typedef std::vector<TypeArityPair> Path;
typedef std::multiset<Path> PathMultiset;
typedef std::map<Handle, PathMultiset> HandlePathsMap;
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
std::string oc_to_string(const FreeVariables& var,
                         const std::string& indent=empty_string);

/** @}*/
}

#endif // _OPENCOG_FREE_VARIABLES_H
