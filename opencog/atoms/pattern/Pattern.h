/*
 * Pattern.h
 *
 * Author: Linas Vepstas April 2015
 *
 * Copyright (C) 2015 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_PATTERN_H
#define _OPENCOG_PATTERN_H

#include <map>
#include <set>
#include <stack>
#include <unordered_map>
#include <vector>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>  // for typedef Type
#include <opencog/atoms/pattern/PatternTerm.h>

namespace opencog {

/** \addtogroup grp_atomspace
 *  @{
 */

/// The Pattern struct contains a low-level analysis of a search pattern,
/// in a format that will make a subsequent search run faster.  It is
/// effectively a "compiled" version of the pattern. Patterns only need
/// to be compiled once; the searches can be performed repeatedly.
///
struct Pattern
{
	/// Private, locally scoped typedefs, not used outside of this class.
	typedef std::unordered_multimap<Handle, PatternTermPtr> ConnectMap;

	/// Each atom of the pattern may appear in several clauses. Moreover,
	/// the same atom may be repeated in the same clause in several
	/// positions. For example:
	///
	///    AndLink
	///       FirstClauseLink
	///          ConceptNode "$x"
	///          ConceptNode "$x"
	///          ConceptNode "$y"
	///       SecondClauseLink
	///          ConceptNode "$x"
	///          ConceptNode "$x"
	///
	/// We need to keep a map from atoms and clauses to the list of
	/// distinct atom occurences. Each distinct occurance is referenced
	/// with a PatternTerm.  Each PatternTerm corresponds to a unique
	/// position in the pattern. Thus, for each Atom, and each clause,
	/// there is at least one, and maybe more PatternTerms. This
	/// collection of PatternTerms is stored in a PatternTermSeq.
	typedef std::pair<Handle, PatternTermPtr> AtomInClausePair;
	typedef std::map<AtomInClausePair, PatternTermSeq> ConnectTermMap;

	Pattern() : have_evaluatables(false), have_virtuals(false) {}

	// -------------------------------------------
	/// The current set of clauses (beta redex context) being grounded.
	std::string redex_name;  // for debugging only!

	/// The original body containing the link (if any).
	Handle           body;

	/// Clauses that might be virtual. User never explictly declared
	/// them one way or the other, so we will have to guess, based on
	/// what's in them. Set by unbundle_clauses().
	HandleSeq        undeclared_clauses;

	/// The mandatory clauses must be satisfied. This includes both
	/// literal clauses and virtual clauses.
	HandleSeq        mandatory;
	PatternTermSeq   pmandatory;

	/// The optional clauses must be ungroundable. They are always
	/// literal, and are never evaluatable or virtual. XXX This member
	/// is mis-named: in the current implementation, the optional
	/// clauses must be literally absent. XXX FIXME rename this member.
	HandleSeq      optionals;
	PatternTermSeq absents;

	/// The always (for-all) clauses have to always be grounded the same
	/// way. Any grounding failure at all invalidates all other groundings.
	PatternTermSeq always;

	/// Evaluatable terms are those that hold a GroundedPredicateNode
	/// (GPN) in them, or are stand-ins (e.g. GreaterThanLink, EqualLink).
	HandleSet evaluatable_terms;   // smallest term that is evaluatable
	bool have_evaluatables;

	/// Evaluatables with two or more variables, bridging across
	/// different pattern components. In principle, these exist only
	/// if the pattern has two or more components; in practice, there
	// may be skew due to imperfect code ... XXX FIXME.
	bool have_virtuals;

	/// Defined terms are terms that are a DefinedPredicateNode (DPN)
	/// or a DefineSchemaNode (DSN).
	HandleSet defined_terms;    // The DPN/DSN itself.

	/// Clauses that can be grounded in only one way; thus the
	/// result of that grounding can be cached, to avoid rechecking.
	/// These clauses cannot contain evaluatable elements (as these
	/// have context-dependent valuations), and can only contain one
	/// variable (for lookup performance.)
	HandleSet cacheable_clauses;

	/// As above, but clauses that hold two or more variables.
	HandleSet cacheable_multi;

	/// For each clause, the list of variables that appear in that clause.
	/// Used in conjunction with the `cacheable_multi` above.
	std::map<PatternTermPtr, HandleSeq> clause_variables;

	/// Any given atom may appear in one or more clauses. Given an atom,
	/// the connectivy map tells you what clauses it appears in. It
	/// captures how the clauses are connected to one-another, so that,
	/// after one clause is solved, we know what parts of the unsolved
	/// clauses already have a solution.
	ConnectMap       connectivity_map;     // setup by make_connectivity_map()

	ConnectTermMap   connected_terms_map;  // setup by make_term_trees()

	std::string to_string(const std::string& indent) const;
};

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Pattern& pattern,
                         const std::string& indent=empty_string);

/** @}*/
} // namespace opencog

#endif // OPENCOG_PATTERN_H
