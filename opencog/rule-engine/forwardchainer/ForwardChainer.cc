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

#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/query/BindLinkAPI.h>
#include <opencog/rule-engine/Rule.h>

#include "ForwardChainer.h"
#include "FocusSetPMCB.h"
#include "../URELogger.h"

using namespace opencog;

ForwardChainer::ForwardChainer(AtomSpace& as, const Handle& rbs,
                               const Handle& source,
                               const Handle& vardecl,
                               const HandleSeq& focus_set,
                               source_selection_mode sm) :
    _as(as), _rec(as), _rbs(rbs), _configReader(as, rbs), _fcstat(as)
{
	_ts_mode = sm;
	init(source, vardecl, focus_set);
}

ForwardChainer::~ForwardChainer()
{
}

void ForwardChainer::init(const Handle& source,
                          const Handle& vardecl,
                          const HandleSeq& focus_set)
{
	validate(source);

	_init_source = source;
	_init_vardecl = vardecl;

	_search_in_af = _configReader.get_attention_allocation();
	_search_focus_set = not focus_set.empty();

	// Set potential source.
	HandleSeq init_sources;

	// Accept set of initial sources wrapped in a SET_LINK.
	if (source->get_type() == SET_LINK) {
		init_sources = source->getOutgoingSet();
	} else {
		init_sources.push_back(source);
	}
	update_potential_sources(init_sources);

	// Add focus set atoms and sources to focus_set atomspace
	if (_search_focus_set) {
		_focus_set = focus_set;

		for (const Handle& h : _focus_set)
			_focus_set_as.add_atom(h);

		for (const Handle& h : _potential_sources)
			_focus_set_as.add_atom(h);
	}

	// Set rules.
	_rules = _configReader.get_rules();
	// TODO: For now the FC follows the old standard. We may move to
	// the new standard when all rules have been ported to the new one.
	for (const Rule& rule : _rules)
		rule.premises_as_clauses = true; // can be modify as mutable

	// Reset the iteration count and max count
	_iteration = 0;
}

UREConfig& ForwardChainer::get_config()
{
	return _configReader;
}

const UREConfig& ForwardChainer::get_config() const
{
	return _configReader;
}

void ForwardChainer::do_chain()
{
	ure_logger().debug("Start Forward Chaining");

	// Relex2Logic uses this. TODO make a separate class to handle
	// this robustly.
	if(_potential_sources.empty())
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
	                     << "/" << _configReader.get_maximum_iterations_str();

	// Expand meta rules. This should probably be done on-the-fly in
	// the select_rule method, but for now it's here
	expand_meta_rules();

	// Select source
	_cur_source = select_source();
	LAZY_URE_LOG_DEBUG << "Selected source:" << std::endl << _cur_source->to_string();

	// Select rule
	Rule rule = select_rule(_cur_source);
	if (not rule.is_valid()) {
		ure_logger().debug("No selected rule, abort step");
		return;
	}

	// Apply rule on _cur_source
	UnorderedHandleSet products = apply_rule(rule);

	// Store results
	update_potential_sources(products);
	_fcstat.add_inference_record(_iteration - 1, // _iteration has
	                                             // already been
	                                             // incremented
	                             _cur_source, rule, products);
}

bool ForwardChainer::termination()
{
	bool terminate = false;

	// Terminate if all sources have been tried (if sources are to be
	// tried only once)
	if (not _configReader.get_retry_sources())
		terminate = 0 < _iteration and _unselected_sources.empty();

	// Terminate if max iterations has been reached
	terminate |= _configReader.get_maximum_iterations() == _iteration;

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
		UnorderedHandleSet uhs = apply_rule(rule);

		// Update
		_fcstat.add_inference_record(_iteration,
		                             _as.add_node(CONCEPT_NODE, "dummy-source"),
		                             rule, uhs);
		update_potential_sources(uhs);
	}
}

UnorderedHandleSet ForwardChainer::get_chaining_result()
{
	return _fcstat.get_all_products();
}

Handle ForwardChainer::select_source()
{
	size_t selsrc_size = _selected_sources.size();
	// If all sources have been selected then insert the sources'
	// children in the set of potential sources
	if (_unselected_sources.empty()) {
		ure_logger().debug() << "All " << selsrc_size
		                     << " sources have already been selected";

		// Hack to help to exhaust sources with
		// multiple matching rules. This would be
		// better used with a memory of which
		// source x rule pairs have been
		// tried. But choose_source would still
		// remain a hack anyway.
		if (biased_randbool(0.01)) {
			for (const Handle& h : _selected_sources) {
				if (h->is_link()) {
					const HandleSeq& outgoings = h->getOutgoingSet();
					HandleSeq no_free_vars_outgoings;
					// Only add children with no free variables in them
					for (const Handle& ch : outgoings)
						if (is_closed(ch))
							no_free_vars_outgoings.push_back(ch);
					update_potential_sources(no_free_vars_outgoings);
				}
			}
			ure_logger().debug() << (_potential_sources.size() - selsrc_size)
			                     << " sources' children have been added as "
			                     << "potential sources";
		} else {
			ure_logger().debug() << "No added sources, "
			                     << "retry existing sources instead";
		}
	}

	ure_logger().debug() << "Selected sources so far "
	                     << selsrc_size << "/" << _potential_sources.size();

	URECommons urec(_as);
	map<Handle, double> tournament_elem;

	const UnorderedHandleSet& to_select_sources =
		_unselected_sources.empty() ? _potential_sources : _unselected_sources;

	// Log selectable sources
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		ss << "Available sources:";
		for (const Handle& source : to_select_sources)
			ss << std::endl << source->id_to_string();
		ure_logger().debug() << ss.str();
	}

	Handle hchosen;
	switch (_ts_mode) {
	case source_selection_mode::TV_FITNESS:
		for (const Handle& s : to_select_sources)
			tournament_elem[s] = urec.tv_fitness(s);
		hchosen = urec.tournament_select(tournament_elem);
		break;

/*
An attentionbank is needed in order to get the STI...
	case source_selection_mode::STI:
	    for (const Handle& s : to_select_sources)
		    tournament_elem[s] = s->getSTI();
	    hchosen = urec.tournament_select(tournament_elem);
	    break;
*/

	case source_selection_mode::UNIFORM:
		hchosen = rand_element(to_select_sources);
		break;

	default:
		throw RuntimeException(TRACE_INFO, "Unknown source selection mode.");
		break;
	}

	OC_ASSERT(hchosen != Handle::UNDEFINED);

	_selected_sources.insert(hchosen);
	_unselected_sources.erase(hchosen);

	return hchosen;
}

Rule ForwardChainer::select_rule(const Handle& source)
{
	URECommons urec(_as);
	std::map<const Rule*, double> rule_weight;
	for (const Rule& r : _rules)
		if (not r.is_meta())
			rule_weight[&r] = urec.tv_fitness(r.get_rule());

	ure_logger().debug("%d rules to be searched as matched against the source",
	                   rule_weight.size());

	// Select a rule among the admissible rules in the rule-base via stochastic
	// selection, based on the weights of the rules in the current context.
	Rule rule;

	while (not rule_weight.empty()) {
		const Rule *temp = _rec.tournament_select(rule_weight);
		ure_logger().fine("Selected rule %s to unify with the source",
		                  temp->get_name().c_str());

		// If the source is the initial source then we may use its
		// variable declaration during rule unification. Otherwise
		// let's for now assume that any variable in a source must be
		// treated as a constant, thus creating an empty VariableList.
		Handle vardecl = source == _init_source ?
			_init_vardecl : Handle(createVariableList(HandleSeq()));

		const AtomSpace& ref_as(_search_focus_set ? _focus_set_as : _as);
		RuleTypedSubstitutionMap urm = temp->unify_source(source, vardecl, &ref_as);
		RuleSet unified_rules = Rule::strip_typed_substitution(urm);

		if (not unified_rules.empty()) {
			// Randomly select a rule amongst the unified ones
			rule = *std::next(unified_rules.begin(),
			                  randGen().randint(unified_rules.size()));

			LAZY_URE_LOG_DEBUG << "The following rule unifies with the source:"
			                   << std::endl << oc_to_string(rule);
			break;
		} else {
			ure_logger().debug("Rule %s is not a match. Looking for another rule",
			                   temp->get_name().c_str());
		}

		rule_weight.erase(temp);
	}

	return rule;
};

UnorderedHandleSet ForwardChainer::apply_rule(const Rule& rule)
{
	UnorderedHandleSet results;

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
		AtomSpace& ref_as(_search_focus_set ? _focus_set_as : _as);
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
			FocusSetPMCB fs_pmcb(&derived_rule_as, &_as);
			fs_pmcb.implicand = bl->get_implicand();
			bl->imply(fs_pmcb, &_focus_set_as, false);
			add_results(_focus_set_as, fs_pmcb.get_result_list());
		}
		// Search the whole atomspace.
		else {
			Handle h = bindlink(&_as, rhcpy);
			add_results(_as, h->getOutgoingSet());
		}
	}
	catch (...) {}

	LAZY_URE_LOG_DEBUG << "Results:" << std::endl << oc_to_string(results);

	return UnorderedHandleSet(results.begin(), results.end());
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
	_rules.expand_meta_rules(_as);

	if (rules_size != _rules.size()) {
		ure_logger().debug() << "The rule set has gone from "
		                     << rules_size << " rules to " << _rules.size();
	}
}
