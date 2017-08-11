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
	rules(rs), _bit(bit), _control_as(control_as)
{
	// Fetch default TVs for each inference rule (the TV on the member
	// link connecting the rule to the rule base
	for (const Rule& rule : rules) {
		_default_tvs[rule.get_alias()] = rule.get_tv();
	}
	std::stringstream ss;
	ss << "Default inference rule TVs:" << std::endl;
	for (const auto& rtv : _default_tvs)
		ss << rtv.second->toString() << " " << oc_to_string(rtv.first);
	ure_logger().debug() << ss.str();

	// Fetches expansion control rules from _control_as
	if (_control_as) {
		for (const Handle& rule_alias : rule_aliases(rules)) {
			HandleSet exp_ctrl_rules = fetch_expansion_control_rules(rule_alias);
			_expansion_control_rules[rule_alias] = exp_ctrl_rules;

			ure_logger().debug() << "Expansion control rules for "
			                     << rule_alias->toString()
			                     << oc_to_string(exp_ctrl_rules);
		}
	}
}

RuleSelection ControlPolicy::select_rule(AndBIT& andbit, BITNode& bitleaf)
{
	// The rule is randomly selected amongst the valid ones, with
	// probability of selection being proportional to its weight.
	const RuleTypedSubstitutionMap valid_rules = get_valid_rules(andbit, bitleaf);
	if (valid_rules.empty()) {
		bitleaf.exhausted = true;
		return RuleSelection();
	}

	// Log all valid rules
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		ss << "The following rules are valid:" << std::endl
		   << oc_to_string(rule_aliases(valid_rules));
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

		// Only insert unexplored rules for this leaf
		RuleTypedSubstitutionMap pos_rules;
		for (const auto& rule : unified_rules)
			if (not _bit.is_in(rule, bitleaf))
				pos_rules.insert(rule);

		valid_rules.insert(pos_rules.begin(), pos_rules.end());
	}
	return valid_rules;
}

RuleSelection ControlPolicy::select_rule(const AndBIT& andbit,
                                         const BITNode& bitleaf,
                                         const RuleTypedSubstitutionMap& inf_rules)
{
	// Build weight vector, based on control rules or otherwise
	// default rule TVs, to do weighted random selection.
	std::vector<double> weights = rule_weights(andbit, bitleaf, inf_rules);

	// Sample an inference rule according to the distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	size_t idx = dist(randGen());
	return {*std::next(inf_rules.begin(), idx), weights[idx]};
}

std::vector<double> ControlPolicy::rule_weights(const AndBIT& andbit,
                                                const BITNode& bitleaf,
                                                const RuleTypedSubstitutionMap& inf_rules)
{
	// For each rule alias, calculate the TV that selecting it will
	// produce a preproof
	HandleTVMap success_tvs;
	for (const auto& rule : rule_aliases(inf_rules)) {
		// Get all active expansion control rules
		HandleSet active_ctrl_rules = active_expansion_control_rules(rule);

		if (active_ctrl_rules.empty()) {
			// If there are no active control rules, use the default
			// TV on the rule.
			success_tvs[rule] = _default_tvs[rule];
		} else {
			// Otherwise calculate the truth value of its mixture
			// model.
			// TODO: set cpx_penalty and compressiveness.
			success_tvs[rule] = MixtureModel(active_ctrl_rules)();
		}
	}

	// Log TVs of representing probability of success (expanding into
	// a preproof) for each action
	std::stringstream ss;
	ss << "Rule TVs:" << std::endl;
	for (const auto& rtv : success_tvs)
		ss << rtv.second->toString() << " " << oc_to_string(rtv.first);
	ure_logger().debug() << ss.str();

	// Given success_tvs calculate the action distribution over rule
	// alias, as to (supposedly) optimally balance exploration and
	// exploitation.
	ActionSelection action_selection(success_tvs);
	HandleCounter alias_weights = action_selection.distribution();

	// Log rule weights for action selection
	std::stringstream ssw;
	ssw << "Rule weights:" << std::endl;
	for (const auto& rw : alias_weights)
		ssw << rw.second << " " << oc_to_string(rw.first);
	ure_logger().debug() << ssw.str();

	// Reweight over rule instances and normalize
	rule_weights(alias_weights, inf_rules);
	std::vector<double> norm_weights = rule_weights(alias_weights, inf_rules);

	return norm_weights;
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

HandleSet ControlPolicy::rule_aliases(const RuleSet& rules) const
{
	HandleSet aliases;
	for (auto& rule : rules)
		aliases.insert(rule.get_alias());
	return aliases;
}

HandleSet ControlPolicy::rule_aliases(const RuleTypedSubstitutionMap& rules) const
{
	HandleSet aliases;
	for (auto& rule : rules)
		aliases.insert(rule.first.get_alias());
	return aliases;
}

HandleSet ControlPolicy::active_expansion_control_rules(const Handle& inf_rule_alias)
{
	if (!_control_as)
		return HandleSet();

	// Filter out inactive expansion control rules
	HandleSet results;
	for (const Handle& ctrl_rule : _expansion_control_rules[inf_rule_alias])
		if (control_rule_active(ctrl_rule))
			results.insert(ctrl_rule);

	// Log active control rules
	std::stringstream ss;
	ss << "Active expansion control rules for "
	   << inf_rule_alias->toString();
	if (results.empty())
		ss << "none";
	else {
		ss << "size = " << results.size();
		for (const Handle& acr : results)
			ss << acr->idToString();
	}
	ure_logger().debug() << ss.str();

	return results;
}

bool ControlPolicy::control_rule_active(const Handle& ctrl_rule) const
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

Handle ControlPolicy::get_expansion_control_rule_pattern(const Handle& ctrl_rule) const
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
