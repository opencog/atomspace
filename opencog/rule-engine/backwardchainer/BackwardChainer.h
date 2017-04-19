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
	                const Handle& target,
	                const Handle& vardecl=Handle::UNDEFINED,
	                const Handle& focus_set=Handle::UNDEFINED,
	                // TODO: maybe wrap all fitnesses in a Fitness class
	                const BITNodeFitness& bitnode_fitness=BITNodeFitness(),
	                const AndBITFitness& andbit_fitness=AndBITFitness());

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
	void expand_meta_rules();

	// Expand the BIT
	void expand_bit();

	// Expand a selected and-BIT. It is not passed by const because it
	// will keep a record of the expansion if successful.
	void expand_bit(AndBIT& andbit);

	// Fulfill the BIT. That is run some or all its and-BITs
	void fulfill_bit();

	// Fulfill an FCS (i.e and-BIT). That is run its forward chaining
	// strategy.
	void fulfill_fcs(const Handle& fcs);

	// Reduce the BIT. Remove some and-BITs.
	void reduce_bit();

	// Pick up an and-BIT randomly, biased so that this and-BIT is
	// unlikely to be expanded for the remainder of the inference.
	void remove_unlikely_expandable_andbit();

	// Calculate distribution based on a (poor) estimate of the
	// probablity of a and-BIT being within the path of the solution.
	std::vector<double> expansion_anbit_weights();

	// Select an and-BIT for expansion
	AndBIT* select_expansion_andbit();

	// Select an and-BIT for fulfilment. Return nullptr if none have
	// been selected.
	const AndBIT* select_fulfillment_andbit() const;

	// Select a valid rule given a target. The selected is a new
	// object because a new rule is created, its variables are
	// uniquely renamed, possibly some partial substitutions are
	// applied.
	//
	// The Selection is random amongst the valid rules and weighted
	// according to their weights.
	//
	// The target is not const because if the rules are exhausted it
	// will set its exhausted flag to false.
	RuleTypedSubstitutionPair select_rule(BITNode& target,
	                                      const Handle& vardecl=Handle::UNDEFINED);
	RuleTypedSubstitutionPair select_rule(const RuleTypedSubstitutionMap& rules);

	// Return all valid rules, in the sense that they may possibly be
	// used to infer the target.
	RuleTypedSubstitutionMap get_valid_rules(const BITNode& target,
	                                         const Handle& vardecl);

	// Return the complexity factor of an andbit. The formula is
	//
	// exp(-complexity_penalty * andbit.complexity())
	double complexity_factor(const AndBIT& andbit) const;

	// Return an very crude estimate of the probability that expanding
	// this and-BIT may lead to a successful inference.
	double operator()(const AndBIT& andbit) const;

	AtomSpace& _as;
	UREConfigReader _configReader;

	// Structure holding the Back Inference Tree
	BIT _bit;

	// TODO: perhaps move that under BIT
	AndBITFitness _andbit_fitness;

	int _iteration;

	// Keep track of the and-BIT of the last expansion. Null if the
	// last expansion has failed.
	const AndBIT* _last_expansion_andbit;

	RuleSet _rules;

	OrderedHandleSet _results;
};


} // namespace opencog

#endif /* BACKWARDCHAINER_H_ */
