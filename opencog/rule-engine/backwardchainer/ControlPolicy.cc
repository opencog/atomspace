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
#include <opencog/query/BindLinkAPI.h>

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

		// Remove rules already in the BIT, or that happen to have a
		// null weight.
		for (RuleTypedSubstitutionMap::iterator it = unified_rules.begin();
		     it != unified_rules.end();) {
			double w = (_bit.is_in(*it, bitleaf) ? 0.0 : 1.0)
				* it->first.get_weight();
			if (w <= 0)
				it = unified_rules.erase(it);
			else
				++it;
		}

		// Reweight and insert
		double N(unified_rules.size());
		for (std::pair<Rule, Unify::TypedSubstitution> pos_rule : unified_rules) {
			// Divide the weights of the rules by their numbers so
		    // that the sum equals the initial rule weight.
			pos_rule.first.set_weight(pos_rule.first.get_weight() / N);
			valid_rules.insert(pos_rule);
		}
	}
	return valid_rules;
}

RuleTypedSubstitutionPair ControlPolicy::select_rule(const AndBIT& andbit,
                                                     const BITNode& bitleaf,
                                                     const RuleTypedSubstitutionMap& rules)
{
	// Build weight vector to do weighted random selection
	std::vector<double> weights;

	// If a control policy atomspace is provided then calculate the
	// distribution according to the provided control rules. Otherwise
	// use the default rule weights.
	if (_control_as)
		weights = learn_rule_weights(andbit, bitleaf, rules);
	else
		for (const auto& rule : rules)
			weights.push_back(rule.first.get_weight());

	// Sample one according to the distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	return rand_element(rules, dist);
}

std::vector<double> ControlPolicy::learn_rule_weights(const AndBIT& andbit,
                                                      const BITNode& bitleaf,
                                                      const RuleTypedSubstitutionMap& rules)
{
	std::vector<double> weights;
	HandleSet ra = rule_aliases(rules);
	for (const Handle& rule : ra)
		HandleSet pattern_free_control_rules =
			fetch_pattern_free_expansion_control_rules(rule);
	// TODO
	return weights;
}

HandleSet ControlPolicy::rule_aliases(const RuleTypedSubstitutionMap& rules)
{
	HandleSet aliases;
	for (auto& rule : rules)
		aliases.insert(rule.first.get_alias());
	return aliases;
}

HandleSet ControlPolicy::fetch_pattern_free_expansion_control_rules(Handle rule)
{
	Handle query = mk_pattern_free_expansion_control_rules_query(rule);
	Handle result = bindlink(_control_as, query);
	HandleSeq outgoings(result->getOutgoingSet());
	HandleSet results(outgoings.begin(), outgoings.end());
	return results;
}

HandleSet ControlPolicy::fetch_pattern_expansion_control_rules(Handle rule)
{
	Handle query = mk_pattern_expansion_control_rules_query(rule);
	Handle result = bindlink(_control_as, query);
	HandleSeq outgoings(result->getOutgoingSet());
	HandleSet results(outgoings.begin(), outgoings.end());
	return results;
}

Handle ControlPolicy::mk_vardecl_vardecl(Handle vardecl_var)
{
	return al(TYPED_VARIABLE_LINK,
	          vardecl_var,
	          al(TYPE_CHOICE,
	             an(TYPE_NODE, "VariableList"),
	             an(TYPE_NODE, "VariableNode"),
	             an(TYPE_NODE, "TypedVariableLink")));
}

Handle ControlPolicy::mk_list_of_args_vardecl(Handle args_var)
{
	return al(TYPED_VARIABLE_LINK,
	          args_var,
	          an(TYPE_NODE, "ListLink"));
}

Handle ControlPolicy::mk_expand_exec(Handle expand_args_var)
{
	Handle expand_schema = an(SCHEMA_NODE,
	                          TraceRecorder::expand_andbit_predicate_name);
	return al(EXECUTION_LINK,
	          expand_schema,
	          al(UNQUOTE_LINK, expand_args_var));
}

Handle ControlPolicy::mk_preproof_eval(Handle preproof_args_var)
{
	Handle preproof_pred = an(PREDICATE_NODE, preproof_predicate_name);
	return al(EVALUATION_LINK,
	          preproof_pred,
	          al(UNQUOTE_LINK, preproof_args_var));
}

Handle ControlPolicy::mk_pattern_free_expansion_control_rules_query(Handle rule)
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

Handle ControlPolicy::mk_pattern_expansion_control_rules_query(Handle rule)
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
