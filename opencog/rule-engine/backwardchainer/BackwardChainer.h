/*
 * BackwardChainer.h
 *
 * Copyright (C) 2014-2016 OpenCog Foundation
 *
 * Authors: Misgana Bayetta <misgana.bayetta@gmail.com>  October 2014
 *          William Ma <https://github.com/williampma>
 *          Nil Geisweiller 2016
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
#ifndef BACKWARDCHAINER_H_
#define BACKWARDCHAINER_H_

#include <opencog/rule-engine/Rule.h>
#include <opencog/rule-engine/UREConfigReader.h>

#include "BIT.h"

class BackwardChainerUTest;

namespace opencog
{

/**
 * TODO: update that comment
 *
 * Backward chaining falls into two cases
 *
 * 1. Truth value query - Given a target atom whose truth value is not
 *    known and a pool of atoms, find a way to estimate the truth
 *    value of the target Atom, via combining the atoms in the pool
 *    using the inference rule. Eg. The target is "Do people breath"
 *
 *    (InheritanceLink people breath)
 *
 *    the truth value of the target is estimated via doing the
 *    inference "People are animals, animals breathe, therefore people
 *    breathe."
 *
 * 2. Variable fulfillment query - Given a target Link (Atoms may be
 *    Nodes or Links) with one or more VariableAtoms among its
 *    targets, figure what atoms may be put in place of these
 *    VariableAtoms, so as to give the grounded targets a high
 *
 *    strength * confidence
 *
 *    Eg. What breathes (InheritanceLink $X breath) can be fulfilled
 *    by pattern matching, whenever there are are multiple values to
 *    fill $X we will use fitness value measure to choose the best
 *    other compound example is what breathes and adds ANDLink
 *    InheritanceLink $X Breath InheritanceLink $X adds
 *
 * Anatomy of current implementation
 * =================================
 *
 * 1. First check if the target matches to something in the knowledge base
 *    already
 *
 * 2. If not, choose an inference Rule R (using some Rule selection criteria)
 *    whose output can unify to the target
 *
 * 3. Reverse ground R's input to restrict the permises search
 *
 * 4. Find all premises that matches the restricted R's input by Pattern
 *    Matching
 *
 * 5. For each set of premises, Forward Chain (a.k.a apply the rule, or
 *    Pattern Matching) on the R to see if it can solve the target.
 *
 * 6. If not, add the permises to the targets list (in addition to some
 *    permise selection criteria)
 *
 * 7. Do target selection and repeat.
 */

class BackwardChainer
{
    friend class ::BackwardChainerUTest;

public:
	BackwardChainer(AtomSpace& as, const Handle& rbs,
	                const Handle& htarget,
	                const Handle& vardecl = Handle::UNDEFINED,
	                const Handle& hfocus_set = Handle::UNDEFINED,
	                const BITFitness& fitness = BITFitness());

	/**
	 * URE configuration accessors
	 */
	UREConfigReader& get_config();
	const UREConfigReader& get_config() const;

	/**
	 * Perform backward chaining inference till the termination
	 * criteria have been met.
	 */
	void do_chain();

	/**
	 * Perform a single backward chaining inference step.
	 */
	void do_step();

	/**
	 * @return true if the termination criteria have been met.
	 */
	bool termination();

	/**
	 * Get the current result on the initial target, a SetLink with
	 * all inferred atoms matching the target.
	 */
	Handle get_results() const;

private:
	// Expand the BIT
	void expand_bit();

	// Expand the BIT given a and-BIT as parent
	void expand_bit(const AndBITFCMap::value_type& andbit);

	// Expand the BIT given a and-BIT as parent, a BITNode as leaf of
	// that and-BIT and an inference rule.
	//
	// TODO: support fitness function.
	void expand_bit(const AndBITFCMap::value_type& andbit,
	                BITNode& leaf, const Rule& rule);

	// Given an atomese forward chaining strategy, a leaf of it to
	// expand, and a rule, return a new forward chaining strategy
	// where the leaf has been substituted by the rule premises and
	// rule application.
	//
	// TODO: give examples.
	Handle expand_fcs(const Handle& fcs, const Handle& leaf, const Rule& rule);

	// Given the pattern term of an atomese forward chaining strategy,
	// the leaf from which to expand and premises, replace the leaf by
	// the premises.
	//
	// TODO: give examples.
	Handle expand_fcs_pattern(const Handle& fcs_pattern,
	                          const Handle& leaf, const HandleSeq& premises);

	// Given the rewrite term of an atomese forward chaining strategy,
	// the leaf from which to expand and a rule rewrite term, replace
	// the leaf by the rule rewrite term.
	//
	// TODO: give examples.
	Handle expand_fcs_rewrite(const Handle& fcs_rewrite,
	                          const Handle& leaf, const Handle& rule_rewrite);

	// Fulfill the BIT. That is run some or all its and-BITs
	void fulfill_bit();

	// Fulfill an and-BIT. That is run its associated forward chaining
	// strategy.
	void fulfill_andbit(const AndBITFCMap::value_type& andbit);

	// Reduce the BIT. Remove some and-BITs.
	void reduce_bit();

	// Select an and-BIT
	const AndBITFCMap::value_type& select_andbit();

	// Select a leaf of an and-BIT for subsequent expansion
	BITNode& select_bitleaf(const AndBITFCMap::value_type& andbit);

	// Select the target to expand
	BITNode* select_target();

	// Select a valid rule given a target. The selected is a new
	// object because a new rule is created, its variables are
	// uniquely renamed, possibly some partial substitutions are
	// applied.
	Rule select_rule(const BITNode& target);

	// Return all valid rules, in the sense that these rules may
	// possibly be used to infer the target.
	RuleSeq get_valid_rules(const BITNode& target);

	// Build the corresponding bitnode of a handle and insert it in
	// _handle2bitnode
	void insert_h2b(const Handle& body, const Handle& vardecl,
	                const BITFitness& fitness);

	// Initialize the _andbits container with
	//
	// {_init_target}
	// ->
	// BindLink
	//   _init_vardecl
	//   _init_target
	//   _init_target
	void init_andbits();

	AtomSpace& _as;
	UREConfigReader _configReader;

	Handle _init_target;
	Handle _init_vardecl;
	BITFitness _init_fitness;

	int _iteration;
	AtomSpace _focus_space;

	// Mapping from handles to their corresponding BITNode
	// bodies. Also where the BITNode are actually instantiated.
	HandleBITNodeMap _handle2bitnode;

	// Collection of and-BITs associated with their forward chaining
	// strategies.
	AndBITFCMap _andbits;

	const std::vector<Rule>& _rules;

	OrderedHandleSet _results;
};


} // namespace opencog

#endif /* BACKWARDCHAINER_H_ */
