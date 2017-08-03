/*
 * ControlPolicy.cc
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

#include "ControlPolicy.h"

#include <opencog/util/random.h>
#include <opencog/util/algorithm.h>
#include <opencog/query/BindLinkAPI.h>

#include "MixtureModel.h"
#include "ActionSelection.h"

#include "TraceRecorder.h"
#include "../URELogger.h"

using namespace opencog;

#define al _query_as.add_link
#define an _query_as.add_node

ControlPolicy::ControlPolicy(const RuleSet& rs, const BIT& bit,
                             AtomSpace* control_as) :
	rules(rs), _bit(bit), _control_as(control_as) {}

RuleTypedSubstitutionPair ControlPolicy::select_rule(AndBIT& andbit,
                                                     BITNode& bitleaf)
{
	// The rule is randomly selected amongst the valid ones, with
	// probability of selection being proportional to its weight.
	const RuleTypedSubstitutionMap valid_rules = get_valid_rules(andbit, bitleaf);
	if (valid_rules.empty()) {
		bitleaf.exhausted = true;
		return {Rule(), Unify::TypedSubstitution()};;
	}

	// Log all valid rules and their weights
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		ss << "The following weighted rules are valid:";
		for (const auto& r : valid_rules)
			ss << std::endl << r.first.get_weight() << " " << r.first.get_name();
		LAZY_URE_LOG_DEBUG << ss.str();
	}

	return select_rule(andbit, bitleaf, valid_rules);
}

RuleTypedSubstitutionMap ControlPolicy::get_valid_rules(const AndBIT& andbit,
                                                        const BITNode& bitleaf)
{
	// Generate all valid rules
	RuleTypedSubstitutionMap valid_rules;
	for (const Rule& rule : rules) {
		// For now ignore meta rules as they are forwardly applied in
		// expand_bit()
		if (rule.is_meta())
			continue;

		// Get the leaf vardecl from fcs. We don't want to filter it
		// because otherwise the typed substitution obtained may miss some
		// variables in the FCS declaration that needs to be substituted
		// during expension.
		Handle vardecl;
		if (andbit.fcs)
			vardecl = BindLinkCast(andbit.fcs)->get_vardecl();

		RuleTypedSubstitutionMap unified_rules
			= rule.unify_target(bitleaf.body, vardecl);

		// Insert only rules with positive probability of success
		RuleTypedSubstitutionMap pos_rules;
		for (const auto& rule : unified_rules) {
			double p = (_bit.is_in(rule, bitleaf) ? 0.0 : 1.0)
				* rule.first.get_weight();
			if (p > 0) pos_rules.insert(rule);
		}

		valid_rules.insert(pos_rules.begin(), pos_rules.end());
	}
	return valid_rules;
}

RuleTypedSubstitutionPair ControlPolicy::select_rule(const AndBIT& andbit,
                                                     const BITNode& bitleaf,
                                                     const RuleTypedSubstitutionMap& inf_rules)
{
	// Build weight vector to do weighted random selection
	std::vector<double> weights;

	// If a control policy atomspace is provided then calculate the
	// distribution according to the provided control rules.
	// Otherwise use the default rule weights. Alternatively this
	// could be a linear combination of the two.
	if (_control_as)
		weights = control_rule_weights(andbit, bitleaf, inf_rules);
	else
		weights = default_rule_weights(inf_rules);

	// Sample one according to the distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	return rand_element(inf_rules, dist);
}

std::vector<double> ControlPolicy::default_rule_weights(const RuleTypedSubstitutionMap& inf_rules)
{
	return rule_weights(default_alias_weights(inf_rules), inf_rules);
}

std::vector<double> ControlPolicy::control_rule_weights(
	const AndBIT& andbit, const BITNode& bitleaf,
	const RuleTypedSubstitutionMap& inf_rules)
{
	// For each rule alias, calculate the TV that selecting it will
	// produce a preproof
	HandleTVMap success_tvs;
	for (const Handle& rule : rule_aliases(inf_rules)) {
		// Get all active expansion control rules
		HandleSet active_ctrl_rules =
			fetch_active_expansion_control_rules(rule);

		// Calculate the truth value of its mixture model
		// TODO: set cpx_penalty and compressability.
		success_tvs[rule] = MixtureModel(active_ctrl_rules)();
	}

	// Given success_tvs calculate the action distribution over rule
	// alias, as to (supposedly) optimally balance exploration and
	// exploitation.
	ActionSelection action_selection(success_tvs);
	HandleCounter alias_weights = action_selection.distribution();

	// Reweight over rule instances
	return rule_weights(alias_weights, inf_rules);
}

std::vector<double> ControlPolicy::rule_weights(
	const HandleCounter& alias_weights,
	const RuleTypedSubstitutionMap& inf_rules) const
{
	// Count how many rules has each alias
	HandleCounter alias_counter;
	for (const auto& rule : inf_rules)
		alias_counter[rule.first.get_alias()] += 1.0;

	// Divide the weight of each rule by its alias count
	std::vector<double> weights;
	for (const auto& rule : inf_rules) {
		const Handle& alias = rule.first.get_alias();
		weights.push_back(alias_weights.get(alias) / alias_counter[alias]);
	}

	return weights;
}

HandleSet ControlPolicy::rule_aliases(const RuleTypedSubstitutionMap& rules) const
{
	HandleSet aliases;
	for (auto& rule : rules)
		aliases.insert(rule.first.get_alias());
	return aliases;
}

HandleCounter ControlPolicy::default_alias_weights(const RuleTypedSubstitutionMap& rules) const
{
	HandleCounter res;
	for (auto& rule : rules)
		res[rule.first.get_alias()] = rule.first.get_weight();
	return res;
}

HandleSet ControlPolicy::fetch_active_expansion_control_rules(const Handle& inf_rule)
{
	HandleSet results;
	for (const Handle& ctrl_rule : fetch_expansion_control_rules(inf_rule))
		if (control_rule_active(ctrl_rule))
			results.insert(ctrl_rule);
	return results;
}

bool ControlPolicy::control_rule_active(const Handle& ctrl_rule)
{
	// For now we assume that it is an expansion control rule
	Handle expansion_pattern = get_expansion_control_rule_pattern(ctrl_rule);
	if (expansion_pattern) {
		// TODO
		OC_ASSERT(false, "Not implemented");
		return true;
	}

	return true;
}

Handle ControlPolicy::get_expansion_control_rule_pattern(const Handle& ctrl_rule)
{
	// Check that it is indeed an expansion control rule
	OC_ASSERT(ctrl_rule->getType() == IMPLICATION_SCOPE_LINK);
	OC_ASSERT(ctrl_rule->getArity() == 3);

	// The pattern is in the implicant, if any
	Handle implicant = ctrl_rule->getOutgoingAtom(1);

	// If present it must be inside a conjunction of an ExecutionLink
	// and a pattern
	if (implicant->getType() == AND_LINK)
		for (const Handle& child : implicant->getOutgoingSet())
			if (child->getType() != EXECUTION_LINK)
				return child;

	return Handle::UNDEFINED;
}

HandleSet ControlPolicy::fetch_expansion_control_rules(const Handle& inf_rule)
{
	return set_union(fetch_pattern_free_expansion_control_rules(inf_rule),
	                 fetch_pattern_expansion_control_rules(inf_rule));
}

HandleSet ControlPolicy::fetch_pattern_free_expansion_control_rules(const Handle& inf_rule)
{
	Handle query = mk_pattern_free_expansion_control_rules_query(inf_rule);
	Handle result = bindlink(_control_as, query);
	HandleSeq outgoings(result->getOutgoingSet());
	HandleSet results(outgoings.begin(), outgoings.end());
	return results;
}

HandleSet ControlPolicy::fetch_pattern_expansion_control_rules(const Handle& inf_rule)
{
	Handle query = mk_pattern_expansion_control_rules_query(inf_rule);
	Handle result = bindlink(_control_as, query);
	HandleSeq outgoings(result->getOutgoingSet());
	HandleSet results(outgoings.begin(), outgoings.end());
	return results;
}

Handle ControlPolicy::mk_vardecl_vardecl(const Handle& vardecl_var)
{
	return al(TYPED_VARIABLE_LINK,
	          vardecl_var,
	          al(TYPE_CHOICE,
	             an(TYPE_NODE, "VariableList"),
	             an(TYPE_NODE, "VariableNode"),
	             an(TYPE_NODE, "TypedVariableLink")));
}

Handle ControlPolicy::mk_list_of_args_vardecl(const Handle& args_var)
{
	return al(TYPED_VARIABLE_LINK,
	          args_var,
	          an(TYPE_NODE, "ListLink"));
}

Handle ControlPolicy::mk_expand_exec(const Handle& expand_args_var)
{
	Handle expand_schema = an(SCHEMA_NODE,
	                          TraceRecorder::expand_andbit_predicate_name);
	return al(EXECUTION_LINK,
	          expand_schema,
	          al(UNQUOTE_LINK, expand_args_var));
}

Handle ControlPolicy::mk_preproof_eval(const Handle& preproof_args_var)
{
	Handle preproof_pred = an(PREDICATE_NODE, preproof_predicate_name);
	return al(EVALUATION_LINK,
	          preproof_pred,
	          al(UNQUOTE_LINK, preproof_args_var));
}

Handle ControlPolicy::mk_pattern_free_expansion_control_rules_query(const Handle& inf_rule)
{
	Handle vardecl_var = an(VARIABLE_NODE, "$vardecl"),
		vardecl_vardecl = mk_vardecl_vardecl(vardecl_var),

		expand_args_var = an(VARIABLE_NODE, "$expand_args"),
		expand_args_vardecl = mk_list_of_args_vardecl(expand_args_var),
		expand_exec = mk_expand_exec(expand_args_var),

		preproof_args_var = an(VARIABLE_NODE, "$preproof_args"),
		preproof_args_vardecl = mk_list_of_args_vardecl(preproof_args_var),
		preproof_eval = mk_preproof_eval(preproof_args_var),

		// ImplicationScope to retrieve
		expand_preproof_impl = al(QUOTE_LINK,
		                          al(IMPLICATION_SCOPE_LINK,
		                             al(UNQUOTE_LINK, vardecl_var),
		                             expand_exec,
		                             preproof_eval)),

		// BindLink of ImplicationScope to retrieve
		expand_preproof_impl_bl = al(BIND_LINK,
		                             al(VARIABLE_LIST,
		                                vardecl_vardecl,
		                                expand_args_vardecl,
		                                preproof_args_vardecl),
		                             expand_preproof_impl,
		                             expand_preproof_impl);

	return expand_preproof_impl_bl;
}

Handle ControlPolicy::mk_pattern_expansion_control_rules_query(const Handle& inf_rule)
{
	Handle vardecl_var = an(VARIABLE_NODE, "$vardecl"),
		vardecl_vardecl = mk_vardecl_vardecl(vardecl_var),

		expand_args_var = an(VARIABLE_NODE, "$expand_args"),
		expand_args_vardecl = mk_list_of_args_vardecl(expand_args_var),
		expand_exec = mk_expand_exec(expand_args_var),

		preproof_args_var = an(VARIABLE_NODE, "$preproof_args"),
		preproof_args_vardecl = mk_list_of_args_vardecl(preproof_args_var),
		preproof_eval = mk_preproof_eval(preproof_args_var),

		// ImplicationScope with a pattern in its antecedent to
		// retrieve
		pattern_var = an(VARIABLE_NODE, "$pattern"),
		pat_expand_preproof_impl = al(QUOTE_LINK,
		                              al(IMPLICATION_SCOPE_LINK,
		                                 al(UNQUOTE_LINK, vardecl_var),
		                                 al(AND_LINK,
		                                    expand_exec,
		                                    al(UNQUOTE_LINK, pattern_var)),
		                                 preproof_eval)),

		// Bind of ImplicationScope with a pattern in its antecedent
		// to retrieve
		pat_expand_preproof_impl_bl = al(BIND_LINK,
		                                 al(VARIABLE_LIST,
		                                    vardecl_vardecl,
		                                    expand_args_vardecl,
		                                    preproof_args_vardecl,
		                                    pattern_var),
		                                 pat_expand_preproof_impl,
		                                 pat_expand_preproof_impl);

	return pat_expand_preproof_impl_bl;
}

#undef al
#undef an
