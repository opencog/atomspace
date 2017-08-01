/*
 * ControlPolicy.h
 *
 * Copyright (C) 2017 OpenCog Foundation
 *
 * Authors: Nil Geisweiller
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
#ifndef OPENCOG_CONTROLPOLICY_H_
#define OPENCOG_CONTROLPOLICY_H_

#include <opencog/atomspace/AtomSpace.h>

#include "BIT.h"
#include "../Rule.h"

namespace opencog
{

class ControlPolicy
{
public:
	ControlPolicy(const RuleSet& rules, const BIT& bit,
	              AtomSpace* control_as=nullptr);

	const std::string preproof_predicate_name = "URE:BC:preproof";

	RuleSet rules;

	/**
	 * Select a valid rule given a target. The selected is a new
	 * object because a new rule is created, its variables are
	 * uniquely renamed, possibly some partial substitutions are
	 * applied.
	 *
	 * Unless a control_as is provided at construction time, the
	 * Selection is random amongst the valid rules and weighted
	 * according to their weights.
	 *
	 * TODO: add comments about inference control policy, see
	 * README.md of
	 * <OPENCOG_ROOT>/examples/pln/inference-control-learning
	 *
	 * The andbit and bitleaf are not const because if the rules are
	 * exhausted it will set its exhausted flag to false.
	 */
	RuleTypedSubstitutionPair select_rule(AndBIT& andbit, BITNode& bitleaf);

private:
	// Reference to the BackwardChainer BIT
	const BIT& _bit;

	// AtomSpace holding the inference control rules (or simply
	// control rules for short).
	AtomSpace* _control_as;

	// AtomSpace holding the pattern matcher queries to fetch the
	// various control rule
	AtomSpace _query_as;
	
	/**
	 * Return all valid rules, in the sense that they may possibly be
	 * used to infer the target.
	 */
	RuleTypedSubstitutionMap get_valid_rules(const AndBIT& andbit,
	                                         const BITNode& bitleaf);

	/**
	 * Select a rule for expansion amongst a set of valid ones.
	 */
	RuleTypedSubstitutionPair select_rule(const AndBIT& andbit,
	                                      const BITNode& bitleaf,
	                                      const RuleTypedSubstitutionMap& rules);

	/**
	 * Given a list of valid rules, use the control rules in
	 * _control_as to infer/learn the weights of the optimal policy
	 * (in Thomson sampling sense).
	 *
	 * TODO: add documentation (taken from the pln
	 * inference-control-learning README.md example).
	 */
	std::vector<double> learn_rule_weights(const AndBIT& andbit,
	                                       const BITNode& bitleaf,
	                                       const RuleTypedSubstitutionMap& rules);

	/**
	 * Return the set of rule aliases, as aliases of inference rules
	 * are used in control rules.
	 */
	HandleSet rule_aliases(const RuleTypedSubstitutionMap& rules);

	/**
	 * Fetch control rules from _control_as involved in BIT
	 * expansion. Informally that if and-BIT, A, expands into B from L
	 * with the given rule then B has a probability TV of being a
	 * preproof of T. Formally
	 *
	 * ImplicationScope <TV>
	 *  VariableList
	 *    Variable "$T"  ;; Theorem/target to prove
	 *    TypedVariable  ;; and-BIT to expand
	 *      Variable "$A"
	 *      Type "BindLink"
	 *    Variable "$L"  ;; Leaf from A to expand
	 *    TypedVariable  ;; Resulting and-BIT from the expansion of L from A with rule R
	 *      Variable "$B"
	 *      Type "BindLink"
	 *  Execution
	 *    Schema "expand-and-BIT"
	 *    List
	 *      BontExec Variable "$A"
	 *      Variable "$L"
	 *      DontExec <rule>
	 *    DontExec Variable "$B"
	 *  Evaluation
	 *    Predicate "preproof"
	 *    List
	 *      DontExec Variable "$B"
	 *      Variable "$T"
	 *
	 * Although this rule is likely to be very useful in practice we
	 * still keep it as it may provide some initial guidance when no
	 * predictive patterns have been extracted from the inference
	 * history so far. It may also provide a reference when building
	 * the mixture model in case the so called patterns are actually
	 * overfit.
	 */
	HandleSet fetch_pattern_free_expansion_control_rules(Handle rule);

	/**
	 * Fetch control rules from _control_as involved in BIT
	 * expansion. Informally that if and-BIT, A, expands into B from L
	 * with the given rule, and follow some pattern, then B has a
	 * probability TV of being a preproof of T. Formally
	 *
	 * ImplicationScope <TV>
	 *  VariableList
	 *    Variable "$T"  ;; Theorem/target to prove
	 *    TypedVariable  ;; and-BIT to expand
	 *      Variable "$A"
	 *      Type "BindLink"
	 *    Variable "$L"  ;; Leaf from A to expand
	 *    TypedVariable  ;; Resulting and-BIT from the expansion of L from A with rule R
	 *      Variable "$B"
	 *      Type "BindLink"
	 *  And
	 *    Execution
	 *      Schema "expand-and-BIT"
	 *      List
	 *        BontExec Variable "$A"
	 *        Variable "$L"
	 *        DontExec <rule>
	 *      DontExec Variable "$B"
	 *    <pattern>
	 *  Evaluation
	 *    Predicate "preproof"
	 *    List
	 *      DontExec Variable "$B"
	 *      Variable "$T"
	 */
	HandleSet fetch_pattern_expansion_control_rules(Handle rule);

	/**
	 * Helpers to build various hypergraphs used to build various queries
	 */
	Handle mk_vardecl_vardecl(Handle vardecl_var);
	Handle mk_list_of_args_vardecl(Handle args_var);
	Handle mk_expand_exec(Handle expand_args_var);
	Handle mk_preproof_eval(Handle preproof_args_var);
	Handle mk_pattern_free_expansion_control_rules_query(Handle rule);
	Handle mk_pattern_expansion_control_rules_query(Handle rule);
};


} // namespace opencog

#endif /* OPENCOG_CONTROLPOLICY_H_ */
