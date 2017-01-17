/*
 * BIT.h
 *
 * Copyright (C) 2016-2017 OpenCog Foundation
 * Author: Nil Geisweiller
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

#ifndef _OPENCOG_BIT_H
#define _OPENCOG_BIT_H

#include <random>
#include <boost/operators.hpp>
#include <opencog/rule-engine/Rule.h>
#include <opencog/atoms/base/Handle.h>
#include "Fitness.h"

namespace opencog
{

/**
 * A BIT (Back Inference Tree) node, and how it relates to its
 * children. A back-inference tree is an and-or-tree, where there are
 * 2 types of children, or-children and and-children. The or-children
 * are represented by BITNode::rules, because multiple rules or rule
 * variations can infer the same target. Then within each rule or rule
 * variation, the rule premises are and-children because in order to
 * apply a certain rule all premises must be fulfilled.
 */
class BITNode
{
public:
	BITNode(const Handle& body=Handle::UNDEFINED,
	        const BITNodeFitness& fitness=BITNodeFitness());

	// BITNode handle (TODO: maybe this is not necessary)
	Handle body;

	// BITNode fitness
	BITNodeFitness fitness;

	// Or-children at the rule level, as multiple rules, or rule
	// variations (partially unified, etc) can yield the same target.
	RuleSet rules;

	// True iff all valid rules have already expanded this BIT-node.
	// TODO (don't forget to reset if the rule set changes)
	bool exhausted;

	// Estimate the probability of usefulness of expanding this
	// BIT-Node.
	// TODO: Maybe this should be moved to BackwardChainer
	double operator()() const;

	std::string to_string() const;
};

/**
 * And-BIT
 */
class AndBIT : public boost::less_than_comparable<AndBIT>,
               public boost::equality_comparable<AndBIT>
{
public:
	// FCS associated to the and-BIT
	Handle fcs;

	// Mapping from the FCS leaves to BITNodes
	typedef std::unordered_map<Handle, BITNode> HandleBITNodeMap;
	HandleBITNodeMap leaf2bitnode;

	// True iff all leaves are exhausted (see BITNode::exhausted)
	// TODO (don't forget to reset if the rule set changes)
	bool exhausted;

	/**
	 * @brief Initialize an and-BIT with a certain target, vardecl and
	 * fitness and add it in as.
	 */
	AndBIT();
	AndBIT(AtomSpace& as, const Handle& target, const Handle& vardecl,
	       const BITNodeFitness& fitness=BITNodeFitness());
	AndBIT(const Handle& fcs);
	~AndBIT();

	/**
	 * @brief Expand the and-BIT given a target leaf and rule.
	 *
	 * @param leaf from which to expand the and-BIT.
	 * @param rule with which to expand the and-BIT.
	 *
	 * @return A new and-BIT resulting from the expansion.
	 *
	 * @todo support fitness function.
	 */
	AndBIT expand(const Handle& leaf, const Rule& rule) const;

	/**
	 * @brief Randomly select a leaf of the FCS. Leaves with lower
	 * BIT-node fitness have more chance of being selected (cause they
	 * need to get fitter).
	 *
	 * @return The selected leaf.
	 */
	BITNode* select_leaf();

	/**
	 * Set the and-BIT exhausted flags to false. Take care of the
	 * BIT-nodes exhausted flags as well.
	 */
	void reset_exhausted();

	/**
	 * Comparison operators. For operator< compare fcs by size, or by
	 * handle value if they are of the same size.
	 */
	bool operator==(const AndBIT& andbit) const;
	bool operator<(const AndBIT& andbit) const;

	std::string to_string() const;

private:
	// Weighted distribution over the targets leaves, defined
	// according to their BIT-node fitnesses. The higher the fitness
	// the lower the chance of being selected as it is already fit.
	typedef std::discrete_distribution<size_t> LeafDistribution;

	/**
	 * Given an FCS, a leaf of it to expand, and a rule, return a new
	 * FCS where the leaf has been substituted by the rule premises
	 * and rule application.
	 *
	 * A forward chaining strategy is represented according to
	 * https://github.com/opencog/atomspace/issues/903. TODO:
	 * copy/paste the doc here and in the wiki as well.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs(const Handle& leaf, const Rule& rule) const;

	/**
	 * @brief Given that FCS is defined generate the mapping from FCS
	 * leaves to bitnotes.
	 */
	void set_leaf2bitnode();

	/**
	 * @brief Build the bitnode associated to leaf and insert it in
	 * leaf2bitnode.
	 */
	void insert_bitnode(Handle leaf, const BITNodeFitness& fitness);

	/**
	 * Return all the leaves (or blanket because these new target
	 * leaves cover the previous intermediary targets), of an
	 * FCS.
	 */
	OrderedHandleSet get_leaves() const;
	OrderedHandleSet get_leaves(const Handle& h) const;

	/**
	 * Given a FCS, a leaf of it and a rule. Unify the rule conclusion
	 * with the leaf and replace any variables in the FCS by its
	 * corresponding term in the rule.
	 */
	Handle substitute_unified_variables(const Handle& leaf,
	                                    const Rule& rule) const;

	/**
	 * Given the pattern term of an FCS where all variables have been
	 * substituted by the corresponding terms in the rule conclusion,
	 * expand the rule conclusion by its premises.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs_pattern(const Handle& fcs_pattern,
	                          const Rule& rule) const;

	/**
	 * Given the rewrite term of an FCS where all variables have been
	 * substituted by the corresponding terms in the rule conclusion,
	 * replace the rule conclusion by the rule rewrite term.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs_rewrite(const Handle& fcs_rewrite,
	                          const Rule& rule) const;

	/**
	 * Return true if atom is an argument of an evaluation
	 */
	bool is_argument_of(const Handle& eval, const Handle& atom) const;

	/**
	 * Equal even if one of them is locally quoted
	 */
	bool is_locally_quoted_eq(const Handle& lhs, const Handle& rhs) const;
};

/**
 * Back Inference Tree. A graph of BIT-Nodes and a collection of
 * and-BITs (as Forward Chaining Strategies, FCS for short), with
 * methods to build and use it.
 */
class BIT
{
public:
	// Atomspace for storing the BIT
	AtomSpace bit_as;

	// Collection of and-BITs. We use a sorted vector instead of a set
	// because the andbit being expanded is modified (its expanded
	// bit-Node keeps track of the expansion). Alternatively we could
	// use a set and define AndBIT::leaf2bitnode as mutable.
	typedef std::vector<AndBIT> AndBITs;
	AndBITs andbits;

	/**
	 * Ctor/Dtor
	 */
	BIT(AtomSpace& as, const Handle& target, const Handle& vardecl,
	    const BITNodeFitness& fitness=BITNodeFitness());
	~BIT();

	/**
	 * @brief return true iff the BIT is empty (i.e. has no and-BITs).
	 */
	bool empty() const;

	/**
	 * @brief Initialize the BIT and return the initial and-BIT.
	 */
	AndBIT* init();

	/**
	 * Expand the andbit, add it to the BIT and return its pointer. If
	 * the expansion has failed return nullptr.
	 *
	 * andbit and bitleaf are not passed by const because bitleaf
	 * keeps a record of that expansion and is this modified during
	 * that step.
	 */
	AndBIT* expand(AndBIT& andbit, BITNode& bitleaf, const Rule& rule);

	/**
	 * Insert a new andbit in the BIT and return its pointer, nullptr
	 * if not inserted (which may happen if an equivalent one is
	 * already in it).
	 */
	AndBIT* insert(const AndBIT& andbit);

	/**
	 * Erase one more andbits. Return iterator after the erasing has
	 * occured.
	 */
	AndBITs::iterator erase(AndBITs::iterator from, AndBITs::iterator to);

	/**
	 * Reset to false all and-BITs exhausted flags.
	 */
	void reset_exhausted_flags();

	/**
	 * Return true if the rule is already an or-children of bitnode up
	 * to an alpha conversion.
	 */
	bool is_in(const Rule& rule, const BITNode& bitnode) const;

private:
	Handle _init_target;
	Handle _init_vardecl;
	BITNodeFitness _init_fitness;
};

// Gdb debugging, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const BITNode& bitnode);
std::string oc_to_string(const AndBIT& andbit);
// std::string oc_to_string(const BIT& bit);

} // ~namespace opencog

#endif // _OPENCOG_BIT_H
