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

#include <opencog/query/PatternTerm.h>
#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/types.h>  // for typedef Type

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
	// XXX TODO Replace by unordered multimap...
	typedef std::vector<Handle> RootList;
	typedef std::map<Handle, RootList> ConnectMap;
	typedef std::pair<Handle, RootList> ConnectPair;

	// Each atom of the pattern may appear in many clauses. Moreover the same
	// atom may be repeated under the same clause root in many positions.
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

	/// The actual clauses. Set by validate_clauses()
	HandleSeq        clauses;

	// The cnf_clauses are the clauses, but with the AbsentLink removed.
	// This simplifies graph discovery, so that when they are found,
	// they can be rejected (e.g. are not absent).
	HandleSeq        cnf_clauses;  // AbsentLink removed!

	// The mandatory clauses must be grounded.
	HandleSeq        mandatory;

	// The optional clauses don't have to be grounded, but they might be.
	// This is where the absent clauses are held, so e.g. if these do get
	// grounded, they might be rejected (depending on the callback).
	std::set<Handle> optionals;    // Optional clauses

	// Black-box clauses. These are clauses that contain GPN's. These
	// have to drop into scheme or python to get evaluated, which means
	// that they will be slow.  So, we leave these for last, so that the
	// faster clauses can run first, and rule out un-needed evaluations.
	std::set<Handle> black;       // Black-box clauses

	// Evaluatable terms are those that hold a GroundedPredicateNode
	// (GPN) in them, or are stand-ins (e.g. GreaterThanLink, EqualLink).
	std::set<Handle> evaluatable_terms;   // smallest term that is evaluatable
	std::set<Handle> evaluatable_holders; // holds something evaluatable.

	// Execuatable terms are those that hold a GroundedSchemaNode (GSN)
	// in them.
	std::set<Handle> executable_terms;    // smallest term that is executable
	std::set<Handle> executable_holders;  // holds something executable.

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
};

/** @}*/
} // namespace opencog

#endif // OPENCOG_PATTERN_H
