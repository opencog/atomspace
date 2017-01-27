/*
 * BackwardChainer.cc
 *
 * Copyright (C) 2014-2017 OpenCog Foundation
 *
 * Authors: Misgana Bayetta <misgana.bayetta@gmail.com>  October 2014
 *          William Ma <https://github.com/williampma>
 *          Nil Geisweiller 2016-2017
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

#include <random>

#include <boost/range/algorithm/lower_bound.hpp>

#include <opencog/util/random.h>

#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>
#include <opencog/atomutils/Unify.h>
#include <opencog/atomutils/TypeUtils.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/pattern/BindLink.h>

#include <opencog/query/BindLinkAPI.h>

#include "BackwardChainer.h"
#include "BackwardChainerPMCB.h"
#include "UnifyPMCB.h"
#include "BCLogger.h"

using namespace opencog;

BackwardChainer::BackwardChainer(AtomSpace& as, const Handle& rbs,
                                 const Handle& target,
                                 const Handle& vardecl,
                                 const Handle& focus_set, // TODO:
                                                          // support
                                                          // focus_set
                                 const BITNodeFitness& fitness)
	: _fcs_maximum_size(2000),
	  _as(as), _configReader(as, rbs),
	  _bit(as, target, vardecl, fitness),
	  _iteration(0), _last_expansion_andbit(nullptr),
	  _rules(_configReader.get_rules()) {
}

UREConfigReader& BackwardChainer::get_config()
{
	return _configReader;
}

const UREConfigReader& BackwardChainer::get_config() const
{
	return _configReader;
}

void BackwardChainer::do_chain()
{
	while (not termination())
	{
		do_step();
	}
}

void BackwardChainer::do_step()
{
	bc_logger().debug("Iteration %d", _iteration);
	_iteration++;

	expand_bit();
	fulfill_bit();
	reduce_bit();
}

bool BackwardChainer::termination()
{
	return _configReader.get_maximum_iterations() <= _iteration;
}

Handle BackwardChainer::get_results() const
{
	HandleSeq results(_results.begin(), _results.end());
	return _as.add_link(SET_LINK, results);
}

void BackwardChainer::expand_bit()
{
	// This is kinda of hack before meta rules are fully supported by
	// the Rule class.
	size_t rules_size = _rules.size();
	_rules.expand_meta_rules(_as);

	// If the rule set has changed we need to reset the exhausted
	// flags.
	if (rules_size != _rules.size()) {
		_bit.reset_exhausted_flags();
		bc_logger().debug() << "The rule set has gone from "
		                    << rules_size << " rules to " << _rules.size()
		                    << ". All exhausted flags have been reset.";
	}

	// Reset _last_expansion_fcs
	_last_expansion_andbit = nullptr;

	if (_bit.empty()) {
		_last_expansion_andbit = _bit.init();
	} else {
		// Select an FCS (i.e. and-BIT) and expand it
		AndBIT* andbit = select_expansion_andbit();
		LAZY_BC_LOG_DEBUG << "Selected and-BIT for expansion:" << std::endl
		                  << andbit->to_string();
		expand_bit(*andbit);
	}
}

void BackwardChainer::expand_bit(AndBIT& andbit)
{
	// Select leaf
	BITNode* bitleaf = andbit.select_leaf();
	if (bitleaf) {
		LAZY_BC_LOG_DEBUG << "Selected BIT-node for expansion:" << std::endl
		                  << bitleaf->to_string();
	} else {
		bc_logger().debug() << "All BIT-nodes of this and-BIT are exhausted "
		                    << "(or possibly fulfilled). Abort expansion.";
		andbit.exhausted = true;
		return;
	}

	// Build the leaf vardecl from fcs
	Handle vardecl = andbit.fcs.is_defined() ?
		filter_vardecl(BindLinkCast(andbit.fcs)->get_vardecl(), bitleaf->body)
		: Handle::UNDEFINED;

	// Select a valid rule
	Rule rule = select_rule(*bitleaf, vardecl);
	// Add the rule in the _bit.bit_as to make comparing atoms easier
	// as well as logging more consistent.
	rule.add(_bit.bit_as);
	if (not rule.is_valid()) {
		bc_logger().debug("No valid rule for the selected BIT-node, abort expansion");
		return;
	}
	LAZY_BC_LOG_DEBUG << "Selected rule for BIT expansion:" << std::endl
	                  << rule.to_string();

	_last_expansion_andbit = _bit.expand(andbit, *bitleaf, rule);
}

void BackwardChainer::fulfill_bit()
{
	if (_bit.empty()) {
		bc_logger().warn("Cannot fulfill an empty BIT!");
		return;
	}

	// Select an and-BIT for fulfillment
	const AndBIT* andbit = select_fulfillment_andbit();
	if (andbit == nullptr) {
		bc_logger().debug() << "Cannot fulfill an empty and-BIT. "
		                    << "Abort BIT fulfillment";
		return;
	}
	LAZY_BC_LOG_DEBUG << "Selected and-BIT for fulfillment:" << std::endl
	                  << oc_to_string(andbit->fcs);
	fulfill_fcs(andbit->fcs);
}

void BackwardChainer::fulfill_fcs(const Handle& fcs)
{
	// Temporary atomspace to not pollute _as with intermediary
	// results
	AtomSpace tmp_as(&_as);

	// Run the FCS and add the results in _as
	Handle hresult = bindlink(&tmp_as, fcs);
	HandleSeq results;
	for (const Handle& result : hresult->getOutgoingSet())
		results.push_back(_as.add_atom(result));
	LAZY_BC_LOG_DEBUG << "Results:" << std::endl << results;
	_results.insert(results.begin(), results.end());
}

AndBIT* BackwardChainer::select_expansion_andbit()
{
	// Calculate distribution. For now it only uses the complexity
	// factor. Ultimately it should estimate the probability that
	// selecting an andbit for expansion is gonna contribute to the
	// inference.
	std::vector<double> weights;
	for (const AndBIT& andbit : _bit.andbits)
		weights.push_back(operator()(andbit));

	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());

	return &rand_element(_bit.andbits, dist);
}

const AndBIT* BackwardChainer::select_fulfillment_andbit() const
{
	return _last_expansion_andbit;
}

void BackwardChainer::reduce_bit()
{
	// TODO: reset exhausted flags related to the removed and-BITs.

	// Remove and-BITs above a certain size.
	auto complex_lt = [&](const AndBIT& andbit, size_t max_size) {
		return andbit.fcs->size() < max_size; };
	auto it = boost::lower_bound(_bit.andbits, _fcs_maximum_size, complex_lt);
	size_t previous_size = _bit.andbits.size();
	_bit.erase(it, _bit.andbits.end());
	if (size_t removed_andbits = previous_size - _bit.andbits.size()) {
		LAZY_BC_LOG_DEBUG << "Removed " << removed_andbits
		                  << " overly complex and-BITs from the BIT";
	}
}

Rule BackwardChainer::select_rule(BITNode& target, const Handle& vardecl)
{
	// The rule is randomly selected amongst the valid ones, with
	// probability of selection being proportional to its weight.
	const RuleSet valid_rules = get_valid_rules(target, vardecl);
	if (valid_rules.empty()) {
		target.exhausted = true;
		return Rule();
	}

	// Log all valid rules and their weights
	if (bc_logger().is_debug_enabled()) {
		std::stringstream ss;
		ss << "The following rules are valid:";
		for (const Rule& r : valid_rules)
			ss << std::endl << r.get_name();
		LAZY_BC_LOG_DEBUG << ss.str();
	}

	return select_rule(valid_rules);
}

Rule BackwardChainer::select_rule(const RuleSet& rules)
{
	// Build weight vector to do weighted random selection
	std::vector<double> weights;
	for (const Rule& rule : rules)
		weights.push_back(rule.get_weight());

	// No rule exhaustion, sample one according to the distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	return rand_element(rules, dist);
}

RuleSet BackwardChainer::get_valid_rules(const BITNode& target,
                                         const Handle& vardecl)
{
	// Generate all valid rules
	RuleSet valid_rules;
	for (const Rule& rule : _rules) {
		// For now ignore meta rules as they are forwardly applied in
		// expand_bit()
		if (rule.is_meta())
			continue;

		RuleSet unified_rules = rule.unify_target(target.body, vardecl);

		// Insert only rules with positive probability of success
		RuleSet pos_rules;
		for (const Rule& rule : unified_rules) {
			double p = (_bit.is_in(rule, target) ? 0.0 : 1.0) * rule.get_weight();
			if (p > 0) pos_rules.insert(rule);
		}

		valid_rules.insert(pos_rules.begin(), pos_rules.end());
	}
	return valid_rules;
}

double BackwardChainer::complexity_factor(const AndBIT& andbit) const
{
	return exp(-_configReader.get_complexity_penalty() * andbit.fcs->size());
}

double BackwardChainer::operator()(const AndBIT& andbit) const
{
	return (andbit.exhausted ? 0.0 : 1.0) * complexity_factor(andbit);
}
