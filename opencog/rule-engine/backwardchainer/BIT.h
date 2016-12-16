/*
 * BIT.h
 *
 * Authors: William Ma <https://github.com/williampma>
 *          Nil Geisweiller
 *
 * Copyright (C) 2015 OpenCog Foundation
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

#include <opencog/rule-engine/Rule.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{

/**
 * Contains the fitness type of a certain BIT node. For instance
 * whether the target is a variable query such that the variables
 * maximize the target TV in a certain way, etc.
 */
class BITFitness
{
};

/**
 * A BIT (Back Inference Tree) node, and how it relates to its
 * children. A back-inference tree is an and-or-tree, where there are
 * 2 types of children, or-children and and-children. The or-children
 * are represented by Target::rules, because multiple rules or rule
 * variations can infer the same target. Then within each rule or rule
 * variation, the rule premises are and-children because in order to
 * apply a certain rule all premises must be fulfilled.
 */
class BITNode
{
public:
	BITNode(const Handle& body = Handle::UNDEFINED,
	        const Handle& vardecl = Handle::UNDEFINED,
	        const BITFitness& fitness = BITFitness());

	// BITNode handle
	Handle body;

	// BITNode variable declaration
	Handle vardecl;

	// BITNode fitness
	BITFitness fitness;

	// Or-children at the rule level, as multiple rules, or rule
	// variations (partially unified, etc) can yield the same target.
	RuleSet rules;

	std::string to_string() const;
};

/**
 * Mappings from and-tree to forward chaining strategy. The and-tree is
 * represented by its set of leaves.
 *
 * The forward chaining strategy is represented according to
 * https://github.com/opencog/atomspace/issues/903. TODO: copy/paste
 * the doc here and in the wiki as well.
 *
 * TODO: Maybe we don't need the handle set key because it is likely
 * equal to the BindLink pattern of the associated forward chaining
 * strategy.
 */
typedef std::map<OrderedHandleSet, Handle> AndBITFCMap;

/**
 * Mapping from Handle to BITNodePtr in order to quickly access the
 * BITNode of a certain body. This is useful because the premises of a
 * rule are returned in terms of Handle, not BITNode.
 */
typedef std::unordered_map<Handle, BITNode> HandleBITNodeMap;

/**
 * Back Inference Tree. A graph of BIT-Nodes and a collection of
 * and-BITs (as Forward Chaining Strategies, FCS for short), with
 * methods to build and use it.
 */
class BIT
{
public:

	// Constructors

	BIT(AtomSpace& as, const Handle& target, const Handle& vardecl,
	    const BITFitness& fitness=BITFitness());


	// Properties

	bool empty() const;

	// Modifiers

	void init();

	/**
	 * Expand the BIT given a FCS (i.e. and-BIT) as parent, a BITNode
	 * as leaf of that and-BIT and an inference rule.
	 *
	 * Return the FCS created from that expansion, or
	 * Handle::UNDEFINED if the expansion has failed.
	 *
	 * TODO: support fitness function.
	 */
	Handle expand(const Handle& fcs, BITNode& leaf, const Rule& rule);

	// Access

	/**
	 * Select uniformly randomly a FCS amonst the FCS collection
	 */
	Handle select_fcs() const;

	/**
	 * Given a FCS (i.e. and and-BIT) uniformly randomly select a
	 * leave of it as potential target for expansion.
	 */
	BITNode& select_bitleaf(const Handle& fcs);

	// Temporary atomspace for storing the BIT
	AtomSpace bit_as;

private:
	/**
	 * Build the bitnode associated to body and insert it in
	 * _handle2bitnode.
	 */
	void insert_bitnode(Handle body, Handle vardecl, const BITFitness& fitness);

	/**
	 * Initialize the FCS collection with
	 *
	 * BindLink
	 *   _init_vardecl
	 *   _init_target
	 *   _init_target
	 */
	void init_fcss();

	/**
	 * Insert a new FCS in the BIT
	 */
	void insert_fcs(const Handle& fcs);

	/**
	 * Return all the leaves of an FCS (i.e. and-BIT). Another way is
	 * to call it a blanket, because these new target leaves cover the
	 * previous intermediary targets.
	 */
	OrderedHandleSet get_leaves(const Handle& fcs) const;

	/**
	 * Return true if the rule is already an or-children of bitnode up
	 * to an alpha conversion.
	 */
	bool is_in(const Rule& rule, const BITNode& bitnode);

	/**
	 * Given an FCS, a leaf of it to expand, and a rule, return a new
	 * FCS where the leaf has been substituted by the rule premises
	 * and rule application.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs(const Handle& fcs, const Handle& leaf, const Rule& rule);

	/**
	 * Given a FCS, a leaf of it and a rule. Unify the rule conclusion
	 * with the leaf and replace any variables in the FCS by its
	 * corresponding term in the rule.
	 */
	Handle substitute_unified_variables(const Handle& fcs, const Handle& leaf,
	                                    const Rule& rule);

	/**
	 * Given the pattern term of an FCS where all variables have been
	 * substituted by the corresponding terms in the rule conclusion,
	 * expand the rule conclusion by its premises.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs_pattern(const Handle& fcs_pattern, const Rule& rule);

	/**
	 * Given the rewrite term of an FCS where all variables have been
	 * substituted by the corresponding terms in the rule conclusion,
	 * replace the rule conclusion by the rule rewrite term.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs_rewrite(const Handle& fcs_rewrite, const Rule& rule);

	/**
	 * Return true if atom is an argument of an evaluation
	 */
	bool is_argument_of(const Handle& eval, const Handle& atom);

	/**
	 * Equal even if one of them is locally quoted
	 */
	bool is_locally_quoted_eq(const Handle& lhs, const Handle& rhs);

	Handle _init_target;
	Handle _init_vardecl;
	BITFitness _init_fitness;

	// Mapping from handles to their corresponding BITNode
	// bodies. Also where the BITNode are actually instantiated.
	HandleBITNodeMap _handle2bitnode;

	// Set of forward chaining strategies, each one corresponding to
	// an and-BIT.
	OrderedHandleSet _fcss;
};
	
// Gdb debugging, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const BITNode& bitnode);
std::string oc_to_string(const AndBITFCMap& abfc);
std::string oc_to_string(const HandleBITNodeMap& hbn);

} // ~namespace opencog

#endif // _OPENCOG_BIT_H
