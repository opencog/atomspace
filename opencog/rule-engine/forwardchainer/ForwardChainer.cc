/*
 * ForwardChainer.cc
 *
 * Copyright (C) 2014,2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>
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

#include <opencog/util/random.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/rule-engine/Rule.h>

#include "ForwardChainer.h"
#include "FocusSetPMCB.h"
#include "../URELogger.h"
#include "../backwardchainer/ControlPolicy.h"
#include "../ThompsonSampling.h"

using namespace opencog;

ForwardChainer::ForwardChainer(AtomSpace& kb_as,
                               AtomSpace& rb_as,
                               const Handle& rbs,
                               const Handle& source,
                               const Handle& vardecl,
                               const HandleSeq& focus_set)
	: _kb_as(kb_as),
	  _rb_as(rb_as),
	  _config(rb_as, rbs),
	  _sources(_config, source, vardecl),
	  _fcstat(kb_as)
{
	init(source, vardecl, focus_set);
}

ForwardChainer::ForwardChainer(AtomSpace& kb_as,
                               const Handle& rbs,
                               const Handle& source,
                               const Handle& vardecl,
                               const HandleSeq& focus_set)
	: ForwardChainer(kb_as,
	                 rbs->getAtomSpace() ? *rbs->getAtomSpace() : kb_as,
	                 rbs, source, vardecl, focus_set)
{
}

ForwardChainer::~ForwardChainer()
{
}

void ForwardChainer::init(const Handle& source,
                          const Handle& vardecl,
                          const HandleSeq& focus_set)
{
	validate(source);

	_search_focus_set = not focus_set.empty();

	// Add focus set atoms and sources to focus_set atomspace
	if (_search_focus_set) {
		_focus_set = focus_set;

		for (const Handle& h : _focus_set)
			_focus_set_as.add_atom(h);

		for (const Source& src : _sources.sources)
			_focus_set_as.add_atom(src.body);
	}

	// Set rules.
	_rules = _config.get_rules();
	// TODO: For now the FC follows the old standard. We may move to
	// the new standard when all rules have been ported to the new one.
	for (const Rule& rule : _rules)
		rule.premises_as_clauses = true; // can be modify as mutable

	// Reset the iteration count and max count
	_iteration = 0;
}

UREConfig& ForwardChainer::get_config()
{
	return _config;
}

const UREConfig& ForwardChainer::get_config() const
{
	return _config;
}

void ForwardChainer::do_chain()
{
	ure_logger().debug("Start Forward Chaining");
	LAZY_URE_LOG_DEBUG << "With rule set:" << std::endl << oc_to_string(_rules);

	// Relex2Logic uses this. TODO make a separate class to handle
	// this robustly.
	if(_sources.empty())
	{
		apply_all_rules();
		return;
	}

	while (not termination())
	{
		do_step();
	}

	ure_logger().debug("Finished Forward Chaining");
}

void ForwardChainer::do_step()
{
	_iteration++;
	ure_logger().debug() << "Iteration " << _iteration
	                     << "/" << _config.get_maximum_iterations_str();

	// Expand meta rules. This should probably be done on-the-fly in
	// the select_rule method, but for now it's here
	expand_meta_rules();

	// Select source
	Source* source = select_source();
	if (source) {
		LAZY_URE_LOG_DEBUG << "Selected source:" << std::endl
		                   << source->to_string();
	} else {
		LAZY_URE_LOG_DEBUG << "No source selected, abort iteration";
		return;
	}

	// Select rule
	RuleProbabilityPair rule_prob = select_rule(*source);
	const Rule& rule = rule_prob.first;
	double prob(rule_prob.second);
	if (not rule.is_valid()) {
		ure_logger().debug("No selected rule, abort iteration");
		return;
	} else {
		LAZY_URE_LOG_DEBUG << "Selected rule, with probability " << prob
		                   << " of success:" << std::endl << rule.to_string();
	}

	// Apply rule on source
	HandleSet products = apply_rule(rule, *source);

	// Save trace (before inserting new sources because it will cause
	// the source pointer to be invalid).
	_fcstat.add_inference_record(_iteration - 1, source->body, rule, products);

	// Insert the produced sources in the population of sources
	_sources.insert(products, *source, prob);
}

bool ForwardChainer::termination()
{
	bool terminate = false;
	std::string msg;

	// Terminate if all sources have been tried
	if (_sources.exhausted) {
		msg = "all sources have been exhausted";
		terminate = true;
	}
	// Terminate if max iterations has been reached
	else if (_config.get_maximum_iterations() == _iteration) {
		msg = "reach maximum number of iterations";
		terminate = true;
	}

	if (terminate)
		ure_logger().debug() << "Terminate: " << msg;

	return terminate;
}

/**
 * Applies all rules in the rule base.
 *
 * @param search_focus_set flag for searching focus set.
 */
void ForwardChainer::apply_all_rules()
{
	for (const Rule& rule : _rules) {
		ure_logger().debug("Apply rule %s", rule.get_name().c_str());
		HandleSet uhs = apply_rule(rule);

		// Update
		_fcstat.add_inference_record(_iteration,
		                             _kb_as.add_node(CONCEPT_NODE, "dummy-source"),
		                             rule, uhs);
	}
}

HandleSet ForwardChainer::get_chaining_result()
{
	return _fcstat.get_all_products();
}

Source* ForwardChainer::select_source()
{
	std::vector<double> weights = _sources.get_weights();

	// Debug log
	if (ure_logger().is_debug_enabled()) {
		OC_ASSERT(weights.size() == _sources.size());
		std::stringstream ss;
		ss << "Weighted sources (only display positive weights):" << std::endl
		   << "size = " << weights.size();
		for (size_t i = 0; i < weights.size(); i++) {
			if (0 < weights[i]) {
				ss << std::endl << weights[i] << " "
				   << _sources.sources[i].body->id_to_string();
			}
		}
		ure_logger().debug() << ss.str();
	}

	// Calculate the total weight to be sure it's greater than zero
	double total = boost::accumulate(weights, 0.0);

	if (total == 0.0) {
		ure_logger().debug() << "All sources have been exhausted";
		if (_config.get_retry_exhausted_sources()) {
			ure_logger().debug() << "Reset all exhausted flags to retry them";
			_sources.reset_exhausted();
			// Try again
			select_source();
		} else {
			_sources.exhausted = true;
			return nullptr;
		}
	}

	// Sample sources according to this distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	return &rand_element(_sources.sources, dist);
}

RuleSet ForwardChainer::get_valid_rules(const Source& source)
{
	// Generate all valid rules
	RuleSet valid_rules;
	for (const Rule& rule : _rules) {
		// For now ignore meta rules as they are forwardly applied in
		// expand_bit()
		if (rule.is_meta())
			continue;

		const AtomSpace& ref_as(_search_focus_set ? _focus_set_as : _kb_as);
		RuleTypedSubstitutionMap urm =
			rule.unify_source(source.body, source.vardecl, &ref_as);
		RuleSet unified_rules = Rule::strip_typed_substitution(urm);

		// Only insert unexplored rules for this source
		RuleSet pos_rules;
		for (const auto& rule : unified_rules)
			if (not source.is_exhausted(rule))
				pos_rules.insert(rule);

		valid_rules.insert(pos_rules.begin(), pos_rules.end());
	}
	return valid_rules;
}

RuleProbabilityPair ForwardChainer::select_rule(const Handle& h)
{
	Source src(h);
	return select_rule(src);
}

RuleProbabilityPair ForwardChainer::select_rule(Source& source)
{
	const RuleSet valid_rules = get_valid_rules(source);

	// Log valid rules
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		if (valid_rules.empty())
			ss << "No valid rule";
		else
			ss << "The following rules are valid:" << std::endl
			   << valid_rules.to_short_string();
		LAZY_URE_LOG_DEBUG << ss.str();
	}

	if (valid_rules.empty()) {
		source.exhausted = true;
		return {Rule(), 0.0};
	}

	return select_rule(valid_rules);
};

RuleProbabilityPair ForwardChainer::select_rule(const RuleSet& valid_rules)
{
	// Build vector of all valid truth values
	TruthValueSeq tvs;
	for (const Rule& rule : valid_rules)
		tvs.push_back(rule.get_tv());

	// Build action selection distribution
	std::vector<double> weights = ThompsonSampling(tvs).distribution();

	// Log the distribution
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		ss << "Rule weights:" << std::endl;
		size_t i = 0;
		for (const Rule& rule : valid_rules) {
			ss << weights[i] << " " << rule.get_name() << std::endl;
			i++;
		}
		ure_logger().debug() << ss.str();
	}

	// Sample rules according to the weights
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	const Rule& selected_rule = rand_element(valid_rules, dist);

	// Calculate the probability estimate of having this rule fulfill
	// the objective (required to calculate its complexity)
	double prob = BetaDistribution(selected_rule.get_tv()).mean();

	return {selected_rule, prob};
}

HandleSet ForwardChainer::apply_rule(const Rule& rule, Source& source)
{
	// Keep track of rule application to not do it again, and apply rule
	source.rules.insert(rule);
	return apply_rule(rule);
}

HandleSet ForwardChainer::apply_rule(const Rule& rule)
{
	HandleSet results;

	// Take the results from applying the rule, add them in the given
	// AtomSpace and insert them in results
	auto add_results = [&](AtomSpace& as, const HandleSeq& hs) {
		for (const Handle& h : hs)
		{
			Type t = h->get_type();
			// If it's a List or Set then add all the results. That
			// kinda means that to infer List or Set themselves you
			// need to Quote them.
			if (t == LIST_LINK or t == SET_LINK)
				for (const Handle& hc : h->getOutgoingSet())
					results.insert(as.add_atom(hc));
			else
				results.insert(as.add_atom(h));
		}
	};

	// Wrap in try/catch in case the pattern matcher can't handle it
	try
	{
		AtomSpace& ref_as(_search_focus_set ? _focus_set_as : _kb_as);
		AtomSpace derived_rule_as(&ref_as);
		Handle rhcpy = derived_rule_as.add_atom(rule.get_rule());

		if (_search_focus_set) {
			// rule.get_rule() may introduce a new atom that satisfies
			// condition for the output. In order to prevent this
			// undesirable effect, lets store rule.get_rule() in a
			// child atomspace of parent focus_set_as so that PM will
			// never be able to find this new undesired atom created
			// from partial grounding.
			BindLinkPtr bl = BindLinkCast(rhcpy);
			FocusSetPMCB fs_pmcb(&derived_rule_as, &_kb_as);
			fs_pmcb.implicand = bl->get_implicand();
			bl->satisfy(fs_pmcb);
			HandleSeq rslts;
			for (const ValuePtr& v: fs_pmcb.get_result_set())
				rslts.push_back(HandleCast(v));
			add_results(_focus_set_as, rslts);
		}
		// Search the whole atomspace.
		else {
			Handle h = HandleCast(rhcpy->execute(&_kb_as));
			add_results(_kb_as, h->getOutgoingSet());
		}
	}
	catch (...) {}

	LAZY_URE_LOG_DEBUG << "Results:" << std::endl << oc_to_string(results);

	return HandleSet(results.begin(), results.end());
}

void ForwardChainer::validate(const Handle& source)
{
	if (source == Handle::UNDEFINED)
		throw RuntimeException(TRACE_INFO, "ForwardChainer - Invalid source.");
}

void ForwardChainer::expand_meta_rules()
{
	// This is kinda of hack before meta rules are fully supported by
	// the Rule class.
	size_t rules_size = _rules.size();
	_rules.expand_meta_rules(_kb_as);

	if (rules_size != _rules.size()) {
		ure_logger().debug() << "The rule set has gone from "
		                     << rules_size << " rules to " << _rules.size();
	}
}
