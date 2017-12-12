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
#include <opencog/unify/Unify.h>
#include <opencog/atoms/execution/MapLink.h>

#include "MixtureModel.h"
#include "ActionSelection.h"
#include "BetaDistribution.h"

#include "TraceRecorder.h"
#include "../URELogger.h"

using namespace opencog;

#define al _query_as->add_link
#define an _query_as->add_node

ControlPolicy::ControlPolicy(const UREConfig& ure_config, const BIT& bit,
                             AtomSpace* control_as) :
	rules(ure_config.get_rules()), _ure_config(ure_config),
	_bit(bit), _control_as(control_as), _query_as(nullptr)
{
	// Fetch default TVs for each inference rule (the TV on the member
	// link connecting the rule to the rule base
	for (const Rule& rule : rules) {
		_default_tvs[rule.get_alias()] = rule.get_tv();
	}
	std::stringstream ss;
	ss << "Default inference rule TVs:" << std::endl;
	for (const auto& rtv : _default_tvs)
		ss << rtv.second->to_string() << " " << oc_to_string(rtv.first);
	ure_logger().debug() << ss.str();

	// Fetches expansion control rules from _control_as
	if (_control_as) {
		_query_as = new AtomSpace(_control_as);
		for (const Handle& rule_alias : rule_aliases(rules)) {
			HandleSet exp_ctrl_rules = fetch_expansion_control_rules(rule_alias);
			_expansion_control_rules[rule_alias] = exp_ctrl_rules;

			ure_logger().debug() << "Expansion control rules for "
			                     << rule_alias->to_string()
			                     << oc_to_string(exp_ctrl_rules);
		}
	}
}

ControlPolicy::~ControlPolicy()
{
	delete(_query_as);
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
	// Build a mapping from rule to TV of expansion success
	HandleTVMap success_tvs = expansion_success_tvs(andbit, bitleaf, inf_rules);
	std::vector<double> weights = rule_weights(success_tvs, inf_rules);

	// Build weight vector, based on control rules or otherwise
	// default rule TVs, to do weighted random selection.

	// Sample an inference rule according to the distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	size_t idx = dist(randGen());
	const RuleTypedSubstitutionPair& selected_rule = *std::next(inf_rules.begin(), idx);

	// Return the selected rule and its probability of success, will
	// be used to calculate the TV that the produce and-BIT is a
	// preproof.
	double prob = get_actual_mean(success_tvs[selected_rule.first.get_alias()]);
	return {selected_rule, prob};
}

HandleTVMap ControlPolicy::expansion_success_tvs(
	const AndBIT& andbit,
	const BITNode& bitleaf,
	const RuleTypedSubstitutionMap& inf_rules)
{
	// For each rule alias, calculate the TV that selecting it will
	// produce a preproof
	HandleTVMap success_tvs;
	for (const auto& rule : rule_aliases(inf_rules)) {
		// Get all active expansion control rules
		HandleSet active_ctrl_rules =
			active_expansion_control_rules(andbit, bitleaf, rule);

		if (active_ctrl_rules.empty()) {
			// If there are no active control rules, use the default
			// TV on the rule.
			success_tvs[rule] = _default_tvs[rule];
		} else {
			// Otherwise calculate the truth value of its mixture
			// model.
			double cpx_penalty = _ure_config.get_mm_complexity_penalty(),
				compressiveness = _ure_config.get_mm_compressiveness();
			success_tvs[rule] = MixtureModel(active_ctrl_rules,
			                                 cpx_penalty, compressiveness)();
		}
	}

	// Log TVs of representing probability of success (expanding into
	// a preproof) for each action
	std::stringstream ss;
	ss << "Rule TVs of expanding a preproof into another preproof:" << std::endl;
	for (const auto& rtv : success_tvs)
		ss << rtv.second->to_string() << " " << oc_to_string(rtv.first);
	ure_logger().debug() << ss.str();

	return success_tvs;
}


std::vector<double> ControlPolicy::rule_weights(const HandleTVMap& success_tvs,
                                                const RuleTypedSubstitutionMap& inf_rules)
{
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

HandleSet ControlPolicy::active_expansion_control_rules(
	const AndBIT& andbit,
	const BITNode& bitleaf,
	const Handle& inf_rule_alias)
{
	if (!_control_as)
		return HandleSet();

	// Filter out inactive expansion control rules
	HandleSet results;
	for (const Handle& ctrl_rule : _expansion_control_rules[inf_rule_alias])
		if (is_control_rule_active(andbit, bitleaf, ctrl_rule))
			results.insert(ctrl_rule);

	// Log active control rules, if any
	if (not results.empty()) {
		std::stringstream ss;
		ss << "Active expansion control rules for "
		   << inf_rule_alias->to_string()
		   << "size = " << results.size() << ":";
		for (const Handle& acr : results)
			ss << " " << acr->id_to_string();
		ure_logger().debug() << ss.str();
	}

	return results;
}

bool ControlPolicy::is_control_rule_active(const AndBIT& andbit,
                                           const BITNode& bitleaf,
                                           const Handle& ctrl_rule) const
{
	Handle
		// Control rule components
		ctrl_vardecl = ScopeLinkCast(ctrl_rule)->get_vardecl(),
		ctrl_expansion = retrieve_expansion(ctrl_rule),
		ctrl_input = ctrl_expansion->getOutgoingAtom(1),
		ctrl_andbit = ctrl_input->getOutgoingAtom(0),
		ctrl_bitleaf = ctrl_input->getOutgoingAtom(1),
		// Actual components
		actl_andbit = andbit.fcs,
		actl_andbit_vardecl = ScopeLinkCast(actl_andbit)->get_vardecl(),
		actl_bitleaf = bitleaf.body,
		// Wrap the actual andbit in a DontExecLink to match the
		// control rule, and to guaranty that it doesn't get executed
		// while evaluating whether it is active (i.e. whether it
		// matches)
		nexe_actl_andbit = createLink(DONT_EXEC_LINK, actl_andbit);

	// Make sure that the variables in the control rule and the actual
	// andbit are disjoint
	//
	// TODO: should be alpha-converted to have no variable in common.
	Variables ctrl_vars = VariableList(ctrl_vardecl).get_variables(),
		actl_andbit_vars = VariableList(actl_andbit_vardecl).get_variables();
	if (not is_disjoint(ctrl_vars.varset, actl_andbit_vars.varset)) {
		std::stringstream ss;
		ss << "Not implemented yet. "
		   << "ctrl_vars and actual_andbit_vars ctrl_vars should be disjoint, "
		   << "but ctrl_vars = " << oc_to_string(ctrl_vars) << std::endl
		   << "actual_andbit_vars = "
		   << oc_to_string(actl_andbit_vars) << std::endl;
		OC_ASSERT(false, ss.str());
	}

	// Check that the control andbit matches the actual andbit, and
	// that the control bitleaf matches the actual bitleaf.
	return match(ctrl_andbit, nexe_actl_andbit, ctrl_vardecl)
		and match(ctrl_bitleaf, actl_bitleaf, ctrl_vardecl);
}

bool ControlPolicy::match(const Handle& pattern, const Handle& term,
                          const Handle& vardecl) const
{
	AtomSpace tmp_as;
	Handle rewrite = tmp_as.add_node(CONCEPT_NODE, "dummy"),
		impl = tmp_as.add_link(IMPLICATION_SCOPE_LINK,
		                       vardecl, pattern, rewrite),
		tmp_term = tmp_as.add_atom(term),
		result = MapLink(impl, tmp_term).execute(&tmp_as);

	return (bool)result;
}

Handle ControlPolicy::retrieve_expansion(const Handle& ctrl_rule) const
{
	ScopeLinkPtr sc = ScopeLinkCast(ctrl_rule);
	for (const Handle& h : sc->get_body()->getOutgoingSet())
		if (is_expansion(h))
			return h;
	return Handle::UNDEFINED;
}

bool ControlPolicy::is_expansion(const Handle& h) const
{
	if (h->get_type() == EXECUTION_LINK) {
		Handle schema = h->getOutgoingAtom(0);
		return schema->get_name() == TraceRecorder::expand_andbit_schema_name;
	}
	return false;
}

Handle ControlPolicy::get_expansion_control_rule_pattern(const Handle& ctrl_rule) const
{
	// Check that it is indeed an expansion control rule
	OC_ASSERT(ctrl_rule->get_type() == IMPLICATION_SCOPE_LINK);
	OC_ASSERT(ctrl_rule->get_arity() == 3);

	// The pattern is in the implicant, if any
	Handle implicant = ctrl_rule->getOutgoingAtom(1);

	// If present it must be inside a conjunction of an ExecutionLink
	// and a pattern
	if (implicant->get_type() == AND_LINK)
		for (const Handle& child : implicant->getOutgoingSet())
			if (child->get_type() != EXECUTION_LINK)
				return child;

	return Handle::UNDEFINED;
}

HandleSet ControlPolicy::fetch_expansion_control_rules(const Handle& inf_rule)
{
	return set_union(fetch_expansion_control_rules(inf_rule, 0),
	                 fetch_expansion_control_rules(inf_rule, 1));
}

HandleSet ControlPolicy::fetch_expansion_control_rules(const Handle& inf_rule,
                                                       int n)
{
	Handle query = mk_expansion_control_rules_query(inf_rule, n);
	Handle result = bindlink(_control_as, query);
	HandleSeq outgoings(result->getOutgoingSet());
	_control_as->remove_atom(result); // Remove cruft from _control_as
	return HandleSet(outgoings.begin(), outgoings.end());
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

Handle ControlPolicy::mk_expand_exec(const Handle& input_andbit_var,
                                     const Handle& input_leaf_var,
                                     const Handle& inf_rule,
                                     const Handle& output_andbit_var)
{
	Handle expand_schema = an(SCHEMA_NODE, TraceRecorder::expand_andbit_schema_name);
	return al(EXECUTION_LINK,
	          expand_schema,
	          al(LIST_LINK,
	             al(UNQUOTE_LINK, input_andbit_var),
	             al(UNQUOTE_LINK, input_leaf_var),
	             al(DONT_EXEC_LINK, inf_rule)),
	          al(UNQUOTE_LINK, output_andbit_var));
}

Handle ControlPolicy::mk_preproof_eval(const Handle& preproof_args_var)
{
	Handle preproof_pred = an(PREDICATE_NODE, preproof_predicate_name);
	return al(EVALUATION_LINK,
	          preproof_pred,
	          al(UNQUOTE_LINK, preproof_args_var));
}

Handle ControlPolicy::mk_expansion_control_rules_query(const Handle& inf_rule,
                                                       int n)
{
	Handle vardecl_var = an(VARIABLE_NODE, "$vardecl"),
		vardecl_vardecl = mk_vardecl_vardecl(vardecl_var),

		in_preproof_args_var = an(VARIABLE_NODE, "$in_preproof_args"),
		in_preproof_args_vardecl = mk_list_of_args_vardecl(in_preproof_args_var),
		in_preproof_eval = mk_preproof_eval(in_preproof_args_var),

		in_andbit_var = an(VARIABLE_NODE, "$in_andbit"),
		in_leaf_var = an(VARIABLE_NODE, "$in_leaf"),
		out_andbit_var = an(VARIABLE_NODE, "$out_andbit"),
		expand_exec = mk_expand_exec(in_andbit_var, in_leaf_var, inf_rule,
		                             out_andbit_var),

		out_preproof_args_var = an(VARIABLE_NODE, "$out_preproof_args"),
		out_preproof_args_vardecl = mk_list_of_args_vardecl(out_preproof_args_var),
		out_preproof_eval = mk_preproof_eval(out_preproof_args_var);

	HandleSeq pattern_vars = mk_pattern_vars(n);

	// ImplicationScope with a pattern in its antecedent to
	// retrieve
	HandleSeq antecedents{in_preproof_eval, expand_exec};
	for (const Handle pv : pattern_vars)
		antecedents.push_back(al(UNQUOTE_LINK, pv));
	Handle pat_expand_preproof_impl = al(QUOTE_LINK,
	                                     al(IMPLICATION_SCOPE_LINK,
	                                        al(UNQUOTE_LINK, vardecl_var),
	                                        al(AND_LINK, antecedents),
	                                        out_preproof_eval));

	// Bind of ImplicationScope with a pattern in its antecedent
	// to retrieve
	HandleSeq vardecls{vardecl_vardecl,
	                   in_preproof_args_vardecl,
	                   in_andbit_var,
	                   in_leaf_var,
	                   out_andbit_var,
	                   out_preproof_args_vardecl};
	vardecls.insert(vardecls.end(), pattern_vars.begin(), pattern_vars.end());

	Handle pat_expand_preproof_impl_bl = al(BIND_LINK,
	                                        al(VARIABLE_LIST, vardecls),
	                                        pat_expand_preproof_impl,
	                                        pat_expand_preproof_impl);

	return pat_expand_preproof_impl_bl;
}

HandleSeq ControlPolicy::mk_pattern_vars(int n)
{
	HandleSeq pattern_vars;
	for (int i = 0; i < n; i++)
		pattern_vars.push_back(mk_pattern_var(i));
	return pattern_vars;
}

Handle ControlPolicy::mk_pattern_var(int i)
{
	std::string name = std::string("$pattern-") + std::to_string(i);
	return an(VARIABLE_NODE, name);
}

double ControlPolicy::get_actual_mean(TruthValuePtr tv) const
{
	return BetaDistribution(tv).mean();
}

#undef al
#undef an
