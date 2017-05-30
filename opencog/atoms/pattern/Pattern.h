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

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/types.h>  // for typedef Type
#include <opencog/atoms/pattern/PatternTerm.h>

namespace opencog {

/** \addtogroup grp_atomspace
 *  @{
 */

/// The Pattern struct defines a search pattern in a way that makes it
/// easier and faster to work with in C++.  It implements the data that
/// is shared between the various pattern-specification atoms and the
/// pattern matcher.
///
struct Pattern
{
	// Private, locally scoped typedefs, not used outside of this class.
	typedef std::unordered_multimap<Handle, Handle> ConnectMap;

	// Each atom of the pattern may appear in many clauses. Moreover, the same
	// atom may be repeated under the same clause root in several positions.
	// AndLink
	//   FirstClauseLink
	//     ConceptNode "$x"
	//     ConceptNode "$x"
	//     ConceptNode "$y"
	//   SecondClauseLink
	//     ConceptNode "$x"
	//     ConceptNode "$x"
	// We need to keep the mapping from atoms and clauses to the list of atom
	// occurences which are referenced by PatternTermPtr pointers.
	// Each pointer corresponds to unique position in the pattern. The list of
	// pointers is stored in PatternTermSeq. Typically the list contains one
	// element, but it might have more if atom repeats in the same clause.
	typedef std::pair<Handle,Handle> AtomInClausePair;  // first is atom
	typedef std::map<AtomInClausePair, PatternTermSeq> ConnectTermMap;

	// -------------------------------------------
	// The current set of clauses (beta redex context) being grounded.
	std::string redex_name;  // for debugging only!

	/// The original body containing the link (if any).
	Handle           body;

	/// The actual clauses. Set by validate_clauses().
	HandleSeq        clauses;

	/// The removed constant clauses. Set by validate_clauses().
	HandleSeq        constants;

	// The cnf_clauses are the clauses, but with the AbsentLink removed.
	// This simplifies graph discovery, so that when they are found,
	// they can be rejected (e.g. are not absent).
	HandleSeq        cnf_clauses;  // AbsentLink removed!

	// The mandatory clauses must be grounded.
	HandleSeq        mandatory;

	// The optional clauses don't have to be grounded, but they might be.
	// This is where the absent clauses are held, so e.g. if these do get
	// grounded, they might be rejected (depending on the callback).
	OrderedHandleSet optionals;    // Optional clauses

	// Black-box clauses. These are clauses that contain GPN's. These
	// have to drop into scheme or python to get evaluated, which means
	// that they will be slow.  So, we leave these for last, so that the
	// faster clauses can run first, and rule out un-needed evaluations.
	OrderedHandleSet black;       // Black-box clauses

	// Evaluatable terms are those that hold a GroundedPredicateNode
	// (GPN) in them, or are stand-ins (e.g. GreaterThanLink, EqualLink).
	OrderedHandleSet evaluatable_terms;   // smallest term that is evaluatable
	OrderedHandleSet evaluatable_holders; // holds something evaluatable.

	// Executable terms are those that inherit from FunctionLink;
	// this includes ExecutionOutputLink's.
	OrderedHandleSet executable_terms;    // smallest term that is executable
	OrderedHandleSet executable_holders;  // holds something executable.

	// Defined terms are terms that are a DefinedPredicateNode (DPN)
	// or a DefineSchemaNode (DSN).
	OrderedHandleSet defined_terms;    // The DPN/DSN itself.

	// Globby terms are terms that contain a GlobNode
	OrderedHandleSet globby_terms;     // Smallest term that has a glob.

	// Terms that may be grounded in an imprecise way. Similar to a
	// GlobNode, but uses a different algorithm.
	OrderedHandleSet fuzzy_terms;

	// Maps; the value is the largest (evaluatable or executable)
	// term containing the variable. Its a multimap, because
	// a variable may appear in several different evaluatables.
	std::unordered_multimap<Handle,Handle> in_evaluatable;
	std::unordered_multimap<Handle,Handle> in_executable;

	// Any given atom may appear in one or more clauses. Given an atom,
	// the connectivy map tells you what clauses it appears in. It
	// captures how the clauses are connected to one-another, so that,
	// after one clause is solved, we know what parts of the unsolved
	// clauses already have a solution.
	ConnectMap       connectivity_map;     // setup by make_connectivity_map()

	ConnectTermMap   connected_terms_map;  // setup by make_term_trees()

	std::string to_string() const
	{
		std::stringstream ss;
		if (not clauses.empty())
			ss << "clauses:" << std::endl << hs_to_string(clauses);
		if (not constants.empty())
			ss << "constants:" << std::endl << hs_to_string(constants);
		if (not mandatory.empty())
			ss << "mandatory:" << std::endl << hs_to_string(mandatory);
		if (not optionals.empty())
			ss << "optionals:" << std::endl << ohs_to_string(optionals);
		if (not black.empty())
			ss << "black:" << std::endl << ohs_to_string(black);
		if (not evaluatable_terms.empty())
			ss << "evaluatable_terms:" << std::endl
			   << ohs_to_string(evaluatable_terms);
		if (not evaluatable_holders.empty())
			ss << "evaluatable_holders:" << std::endl
			   << ohs_to_string(evaluatable_holders);
		if (not executable_terms.empty())
			ss << "executable_terms:" << std::endl
			   << ohs_to_string(executable_terms);
		if (not executable_holders.empty())
			ss << "executable_holders:" << std::endl
			   << ohs_to_string(executable_holders);
		return ss.str();
	}
};

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Pattern&);

/** @}*/
} // namespace opencog

#endif // OPENCOG_PATTERN_H
