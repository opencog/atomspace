/**
 * opencog/atoms/core/FindUtils.h
 *
 * Utilities for finding atoms in trees.
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas <linasvepstas@gmail.com>
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
 *
 * Created by Linas Vepstas February 2008
 */

#ifndef _OPENCOG_FIND_UTILS_H
#define _OPENCOG_FIND_UTILS_H

#include <set>
#include <vector>

#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/core/ScopeLink.h>

namespace opencog {

/// There are two different types of utilities in this file: those that
/// tell you if some target atom or target atom type occurs in some
/// tree, returning a simple yes/no answer, and another utility, telling
/// you exactly where it occurs.  We start with the more complicated one
/// first.  Otherwise, skip down to get at the simpler ones.

//================================================================
/// The FindAtoms class is used to locate atoms of a given type or
/// target that occur inside some clause.  The target is specified in
/// the constructor, the clause to be searched is given with the
/// 'search_set()' method. After the search has been done, the targets
/// are available in the various public members.
///
/// Find a "target atom", or find all atoms of a given "target type",
/// and all of the links that hold that target, that occur in a clause
/// (an expression tree formed by the outgoing set.) This is typically
/// used to find all variables in an expression, but it can be used for
/// any purpose.
///
/// After search_set() is called, the set of target atoms that were found
/// can be retrieved from the public member `varset`.  The set of links
/// that had one of these targets occurring somewhere, anywhere, within
/// them is in the public member `holders`. This includes the holders of
/// holders, etc, all the way up to the very top.  The `least_holders`
/// member will contain only those links that contain a target in their
/// immediate outgoing set.  That is, the `least_holders` are the
/// smallest links that contain the target.
///
/// A "target" is defined in one of two ways: It is either a specified
/// target type, or it is any one of the given set of specific target
/// atoms.  When only the type is given, it will typically be
/// VARIABLE_NODE, GROUNDED_PREDICATE_NODE or GROUNDED_SCHEMA_NODE,
/// COMPOSE_LINK, or something like that, and so this just finds these
/// atom types if they occur in the clause(s).  Alternately, if a set
/// of specific atoms is given, then this finds the subset of that set
/// that actually occurs in the given clause(s).  That is, it computes
/// the intersection between the set of given atoms, and the set of all
/// atoms that occur in the clauses.
///
/// Note that anything quoted (by the use of
/// QUOTE_LINK/LOCAL_QUOTE_LINK/UNQUOTE_LINK) is not explored.  Thus,
/// a quote acts like a cut, halting recursion until it gets unquoted
/// later on with an UNQUOTE_LINK.
///
class FindAtoms
{
public:
	TypeSet stopset;
	HandleSet varset;
	HandleSet holders;
	HandleSet least_holders;

	FindAtoms(Type t, bool subclass = false);
	FindAtoms(Type ta, Type tb, bool subclass = false);
	FindAtoms(const Handle& atom);
	FindAtoms(const HandleSet& selection);

	/**
	 * Given a handle to be searched, create a set of all of the
	 * target atoms/types that lie in the outgoing set of the handle
	 * (recursively).
	 */
	void search_set(const Handle& h);
	void search_set(const HandleSeq& hlist);
private:
	typedef enum
	{
		NOPE,  // Does not contain.
		YEP,   // Contains, but not immediately so.
		IMM    // Contains immediately below.
	} Loco;

	Loco find_rec(const Handle& h, Quotation quotation=Quotation());

private:
	TypeSet _target_types;
	HandleSet _target_atoms;
};

/**
 * Return true if the indicated atom occurs somewhere in the tree
 * (viz, the tree recursively spanned by the outgoing set of the handle)
 */
bool is_atom_in_tree(const Handle& tree, const Handle& atom);

/**
 * Return true if the indicated atom occurs somewhere in the tree
 * (viz, the tree recursively spanned by the outgoing set of the handle)
 * The search of some is terminated if the predicate `reject` returns
 * true. (That is, rejected subtrees are not searched, so that the
 * rejected subtree is a search-failure).
 */
bool is_found_in_tree(const Handle& tree,
                      const Handle& atom,
                      bool (*reject)(const Handle& tree,
                                     const Handle& subtree,
                                     const Handle& atom));

/**
 * Return true if the indicated atom occurs quoted somewhere in the
 * tree.  That is, it returns true only if the atom is inside a
 * QuoteLink.
 *
 * XXX FIXME: what if it appears quoted in one place, and unquoted
 * in another? then what?
 */
bool is_quoted_in_tree(const Handle& tree, const Handle& atom);

/**
 * Return true if the indicated atom occurs unquoted somewhere in the tree
 * (viz, the tree recursively spanned by the outgoing set of the handle)
 * but ONLY if it is not quoted!  This is meant to be used to search
 * for variables, but only those variables that have not been quoted, as
 * the quoted variables are constants (literals).
 */
bool is_unquoted_in_tree(const Handle& tree, const Handle& atom);

/**
 * Return the minimum quotation level of a given atom. The minimum
 * quotation level of an atom is the minimum number of QuoteLinks
 * minus the number of UnquoteLinks between the root and the atom
 * in the tree (or rather hypergraph).
 *
 * @param tree             handle of the hypergraph to explore
 *
 * @param atom             handle of the atom to check
 *
 * @param quotation        quotation state
 *
 * @return                 minimum quotation level. If atom doesn't appear
 *                         in the tree then it returns the maximum integer.
 *
 * If the number is zero or below, it means that this atom is
 * unquoted at least once. If the number is above zero then it is
 * never unquoted.
 */
int min_quotation_level(const Handle& tree,
                        const Handle& atom,
                        Quotation quotation=Quotation());

/**
 * Return the maximum quotation level of a given atom. The maximum
 * quotation level of an atom is the maximum number of QuoteLinks
 * minus the number of UnquoteLinks between the root and the atom in
 * the tree (or rather hypergraph).
 *
 * @param tree             handle of the hypergraph to explore
 *
 * @param var              handle of the atom to check
 *
 * @param quotation_level  quotation level from the root to the handle
 *                         the atom of tree
 *
 * @return                 maximum quotation level. If var doesn't appear
 *                         in the tree then it returns the minimum integer.
 *
 * If the number is above zero, it means that this atom is quoted
 * at least once. If the number is zero or below then it is never
 * quoted.
 */
int max_quotation_level(const Handle& tree,
                        const Handle& atom,
                        Quotation quotation=Quotation());

/**
 * Return true if the atom (variable) occurs unscoped somewhere in the
 * tree.
 */
bool is_unscoped_in_tree(const Handle& tree, const Handle& atom);

/**
 * Return true if the atom (variable) occurs in a non-executable
 * term somewhere in the tree.
 */
bool is_constant_in_tree(const Handle& tree, const Handle& atom);

/**
 * Return true if the atom (variable) occurs both unquoted and
 * unscoped somewhere in the tree.
*/
bool is_unquoted_unscoped_in_tree(const Handle& tree, const Handle& atom);

/**
 * True if is_unquoted, is_unscoped and is_constant_in_tree
*/
bool is_free_in_tree(const Handle& tree, const Handle& atom);

/**
 * Return true if the atom (variable) occurs both unquoted and
 * unscoped somewhere in any of the trees.
*/
bool is_unquoted_unscoped_in_any_tree(const HandleSeq& trees,
                                      const Handle& atom);

/**
 * Return true if the atom (variable) occurs unquoted and
 * unscoped and constant somewhere in any of the trees.
*/
bool is_free_in_any_tree(const HandleSeq& hs, const Handle& atom);

/**
 * Return true if any of the indicated atoms occur somewhere in
 * the tree (that is, in the tree spanned by the outgoing set.)
 */
bool any_atom_in_tree(const Handle& tree,
                      const HandleSet& atoms);

/**
 * Return true if any of the indicated atoms occur somewhere in
 * the tree (that is, in the tree spanned by the outgoing set.)
 * But ONLY if they are not quoted!  This is intended to be used to
 * search for variables; but when a variable is quoted, it is no
 * longer a variable.
 */
bool any_unquoted_in_tree(const Handle& tree,
                          const HandleSet& atoms);

/**
 * Return true if any of the atoms (variables) occur unscoped
 * somewhere in the tree.
 */
bool any_unscoped_in_tree(const Handle& tree,
                          const HandleSet& atoms);

/**
 * Return true if any of the atoms (variables) occur
 * somewhere in the tree, outside of an executable term.
 */
bool any_constant_in_tree(const Handle& tree,
                          const HandleSet& atoms);

/**
 * Return true if any of the atoms (variables) occur unquoted and
 * unscoped somewhere in the tree.
 */
bool any_unquoted_unscoped_in_tree(const Handle& tree,
                                   const HandleSet& atoms);

/**
 * Return true if any of the atoms (variables) occur unquoted and
 * unscoped and outside of an executable term, somewhere in the tree.
 */
bool any_free_in_tree(const Handle& tree,
                      const HandleSet& atoms);
bool any_free_in_tree(const Handle& tree,
                      const HandleSeq& atoms);

/**
 * Return how many of the indicated atoms occur somewhere in
 * the tree (that is, in the tree spanned by the outgoing set.)
 * But ONLY if they are not quoted or bound! This is intended to
 * be used to search for variables; but when a variable is quoted
 * or bound, it is no longer a free variable.
 */
unsigned int num_unquoted_unscoped_in_tree(const Handle& tree,
                                           const HandleSet& atoms);

/**
 * Return the subset of `atoms` that occur somewhere in the tree
 * that are not quoted or bound. This acts as a filter on the set
 * of atoms; the number in the returned set equals the value
 * returned by `num_unquoted_unscoped_in_tree()` above.
 */
HandleSet unquoted_unscoped_in_tree(const Handle& tree,
                                    const HandleSet& atoms);

/**
 * Return true if the indicated atom occurs somewhere in any of the trees.
 */
bool is_atom_in_any_tree(const HandleSeq& trees,
                         const Handle& atom);

/**
 * Return true if the indicated atom occurs somewhere in any of the trees,
 * but only if it is not quoted.  This is intended to be used to search
 * for variables, which cease to be variable when they are quoted.
 */
bool is_unquoted_in_any_tree(const HandleSeq& trees,
                             const Handle& atom);

/**
 * Returns true if the `clause` contains an unquoted atom of type (or
 * subtype of) `atom_type`.  Quoted terms are constants (literals).
 */
bool contains_atomtype(const Handle& clause, Type atom_type,
                       Quotation quotation=Quotation());

/**
 * Returns a count of the number of times that an unquoted atom of
 * type (or subtype of) `atom_type` appears in `clause`.  Quoted terms
 * are constants (literals).
 */
size_t contains_atomtype_count(const Handle& clause, Type atom_type,
                               Quotation quotation=Quotation());

/**
 * Search for free (unscoped and unquoted) VariableNode in a tree.
 *
 * For example: applying this function over
 *
 *  AndLink
 *     VariableNode "$X"
 *     LambdaLink
 *        VariableNode "$X"
 *        EvaluationLink
 *           PredicateNode "P"
 *           VariableNode "$X"
 *
 * returns {VariableNode "$X"}, because although $X is scoped by the
 * lambda it is also unscoped in the And.
 */
HandleSet get_free_variables(const Handle& h,
                             Quotation quotation=Quotation());
HandleSet get_free_variables(const HandleSeq& hs,
                             Quotation quotation=Quotation());

/**
 * Return the set of all atoms in h, itself and all its descendents.
 */
HandleSet get_all_uniq_atoms(const Handle& h);

/**
 * Return true if h has no free variable (unscoped or unquoted) in it,
 * false otherwise.
 */
bool is_closed(const Handle& h, Quotation quotation=Quotation());

/**
 * Return true only if `h` has no free variable in it, nor does it
 * have any type specifications .. i.e. if `h` is not a deep type.
 */
bool is_constant(const Handle& h, Quotation quotation=Quotation());

} // namespace opencog

#endif // _OPENCOG_FIND_UTILS_H
