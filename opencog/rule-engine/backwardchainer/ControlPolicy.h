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

	// Inference rule set for expanding and-BITs.
	RuleSet rules;

	/**
	 * Select a valid inference rule given a target. The selected is a
	 * new object because a new rule is created, its variables are
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
	//
	// Inference control rules are classified according to the
	// decision of the inference control they affect. For now the
	// following are supported:
	//
	// 1. Expansion Control Rules: for choosing the inference rule to
	//    expand an and-BIT.
	AtomSpace* _control_as;

	// AtomSpace holding the pattern matcher queries to fetch the
	// various control rule
	AtomSpace _query_as;

	// Map each action (inference rule expansion) to the set of
	// control rules involving it.
	std::map<Handle, HandleSet> _expansion_control_rules;

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
	 * Calculate the default weights, as provided by the URE
	 * configuration, for each rule instance.
	 */
	std::vector<double> default_rule_weights(const RuleTypedSubstitutionMap& rules);

	/**
	 * Calculate the rule weights according to the control rules
	 * present in _control_as.
	 */
	std::vector<double> control_rule_weights(const AndBIT& andbit, const BITNode& bitleaf,
	                                         const RuleTypedSubstitutionMap& rules);

	/**
	 * Given the weights (action probability) of each inference rule
	 * alias, return the weights over rule instantiations (unified to
	 * the target). Unifying a rule to a target can lead to multiple
	 * rules, each one will have a equal fraction of weight so that
	 * the sum of weights of all unified rules equals the rule alias
	 * weight.
	 *
	 * Later on this might be replaced by performing action selection
	 * on the rules themselves rather than their aliases.
	 */
	std::vector<double> rule_weights(const HandleCounter& alias_weights,
	                                 const RuleTypedSubstitutionMap& inf_rules) const;

	/**
	 * Return the set of rule aliases, as aliases of inference rules
	 * are used in control rules.
	 */
	HandleSet rule_aliases(const RuleSet& rules) const;
	HandleSet rule_aliases(const RuleTypedSubstitutionMap& rules) const;

	/**
	 * Return the map from rule aliases to their default weights.
	 */
	HandleCounter default_alias_weights(const RuleTypedSubstitutionMap& rules) const;

	/**
	 * Get all active expansion control rules concerning the given
	 * inference rule.
	 */
	HandleSet active_expansion_control_rules(const Handle& inf_rule_alias) const;

	/**
	 * Return true iff the given control is current active, that is
	 * the case of an expansion control rule whether the pattern is
	 * true.
	 */
	bool control_rule_active(const Handle& ctrl_rule) const;

	/**
	 * Return the pattern in a given expansion control rule, if it has
	 * any.
	 */
	Handle get_expansion_control_rule_pattern(const Handle& ctrl_rule) const;

	/**
	 * Given an inference rule, fetch both pattern and pattern free
	 * expansion control rules. See comments of
	 * fetch_pattern_free_expansion_control_rules and
	 * fetch_pattern_expansion_control_rules
	 */
	HandleSet fetch_expansion_control_rules(const Handle& inf_rule);

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
	 *      DontExec <inf_rule>
	 *    DontExec Variable "$B"
	 *  Evaluation
	 *    Predicate "preproof"
	 *    List
	 *      DontExec Variable "$B"
	 *      Variable "$T"
	 *
	 * Although this control rule is not likely to be very useful in
	 * practice we still keep it as it may provide some initial
	 * guidance when no predictive patterns have been extracted from
	 * the inference history so far. It may also provide a reference
	 * when building the mixture model in case the so called patterns
	 * are actually overfit.
	 */
	HandleSet fetch_pattern_free_expansion_control_rules(const Handle& inf_rule);

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
	 *        DontExec <inf_rule>
	 *      DontExec Variable "$B"
	 *    <pattern>
	 *  Evaluation
	 *    Predicate "preproof"
	 *    List
	 *      DontExec Variable "$B"
	 *      Variable "$T"
	 */
	HandleSet fetch_pattern_expansion_control_rules(const Handle& inf_rule);

	/**
	 * Helpers to build various hypergraphs used to build various queries
	 */
	Handle mk_vardecl_vardecl(const Handle& vardecl_var);
	Handle mk_list_of_args_vardecl(const Handle& args_var);
	Handle mk_expand_exec(const Handle& expand_args_var);
	Handle mk_preproof_eval(const Handle& preproof_args_var);
	Handle mk_pattern_free_expansion_control_rules_query(const Handle& inf_rule);
	Handle mk_pattern_expansion_control_rules_query(const Handle& inf_rule);
};


} // namespace opencog

#endif /* OPENCOG_CONTROLPOLICY_H_ */
