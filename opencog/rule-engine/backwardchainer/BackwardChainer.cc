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

#include <opencog/util/random.h>

#include <opencog/query/BindLinkAPI.h>
#include <opencog/unify/Unify.h>

#include "BackwardChainer.h"
#include "BackwardChainerPMCB.h"
#include "../URELogger.h"

using namespace opencog;

BackwardChainer::BackwardChainer(AtomSpace& as, const Handle& rbs,
                                 const Handle& target,
                                 const Handle& vardecl,
                                 AtomSpace* trace_as,
                                 AtomSpace* control_as,
                                 const Handle& focus_set, // TODO:
                                                          // support
                                                          // focus_set
                                 const BITNodeFitness& bitnode_fitness,
                                 const AndBITFitness& andbit_fitness)
	: _as(as), _configReader(as, rbs),
	  _bit(as, target, vardecl, bitnode_fitness),
	  _andbit_fitness(andbit_fitness),
	  _trace_recorder(trace_as),
	  _control(_configReader, _bit, control_as),
	  _rules(_control.rules),
	  _iteration(0), _last_expansion_andbit(nullptr)
{
	// Record the target in the trace atomspace
	_trace_recorder.target(target);
}

UREConfig& BackwardChainer::get_config()
{
	return _configReader;
}

const UREConfig& BackwardChainer::get_config() const
{
	return _configReader;
}

void BackwardChainer::do_chain()
{
	ure_logger().debug("Start Backward Chaining");
	LAZY_URE_LOG_DEBUG << "With rule set:" << std::endl << oc_to_string(_rules);

	while (not termination())
	{
		do_step();
	}

	LAZY_URE_LOG_DEBUG << "Finished Backward Chaining with solutions:"
	                   << std::endl << get_results()->to_string();
}

void BackwardChainer::do_step()
{
	_iteration++;

	ure_logger().debug() << "Iteration " << _iteration
	                     << "/" << _configReader.get_maximum_iterations_str();

	expand_bit();
	fulfill_bit();
	reduce_bit();
}

bool BackwardChainer::termination()
{
	bool terminate = false;
	std::string msg;            // Cause of the termination

	if (_configReader.get_maximum_iterations() == _iteration) {
		msg = "reached the maximum number of iterations";
		terminate = true;
	}
	else if (not _bit.empty() and _bit.andbits_exhausted()) {
		msg = "all AndBITS are exhausted";
		terminate = true;
	}

	if (terminate)
		ure_logger().debug() << "Terminate: " << msg;

	return terminate;
}

Handle BackwardChainer::get_results() const
{
	HandleSeq results(_results.begin(), _results.end());
	return _as.add_link(SET_LINK, results);
}

void BackwardChainer::expand_meta_rules()
{
	// This is kinda of hack before meta rules are fully supported by
	// the Rule class.
	size_t rules_size = _rules.size();
	_rules.expand_meta_rules(_as);

	// If the rule set has changed we need to reset the exhausted
	// flags.
	if (rules_size != _rules.size()) {
		_bit.reset_exhausted_flags();
		ure_logger().debug() << "The rule set has gone from "
		                     << rules_size << " rules to " << _rules.size()
		                     << ". All exhausted flags have been reset.";
	}
}

void BackwardChainer::expand_bit()
{
	// Expand meta rules, before they are fully supported
	expand_meta_rules();

	// Reset _last_expansion_fcs
	_last_expansion_andbit = nullptr;

	if (_bit.empty()) {
		_last_expansion_andbit = _bit.init();
		// Record the initial and-BIT in the trace atomspace
		_trace_recorder.andbit(*_last_expansion_andbit);
	} else {
		// Select an FCS (i.e. and-BIT) and expand it
		AndBIT* andbit = select_expansion_andbit();
		LAZY_URE_LOG_DEBUG << "Selected and-BIT for expansion:" << std::endl
		                   << andbit->to_string();
		expand_bit(*andbit);
	}
}

void BackwardChainer::expand_bit(AndBIT& andbit)
{
	// Select leaf
	BITNode* bitleaf = andbit.select_leaf();
	if (bitleaf) {
		LAZY_URE_LOG_DEBUG << "Selected BIT-node for expansion:" << std::endl
		                   << bitleaf->to_string();
	} else {
		ure_logger().debug() << "All BIT-nodes of this and-BIT are exhausted "
		                     << "(or possibly fulfilled). Abort expansion.";
		andbit.exhausted = true;
		return;
	}

	// Select rule for expansion
	RuleSelection rule_sel = _control.select_rule(andbit, *bitleaf);
	Rule rule(rule_sel.first.first);
	Unify::TypedSubstitution ts(rule_sel.first.second);
	double prob(rule_sel.second);

	// Add the rule in the _bit.bit_as to make comparing atoms easier
	// as well as logging more consistent.
	rule.add(_bit.bit_as);

	// Abort expansion if ill rule
	if (not rule.is_valid()) {
		ure_logger().debug("No valid rule for the selected BIT-node, "
		                   "abort expansion");
		return;
	} else if (rule.has_cycle()) {
		LAZY_URE_LOG_DEBUG << "The following rule has cycle (some premise "
		                   << "equals to conclusion), abort expansion:"
		                   << std::endl << rule.to_string();
		return;
	}

	// Rule seems well, expand
	LAZY_URE_LOG_DEBUG << "Selected rule, with probability " << prob
	                   << " of success for BIT expansion:" << std::endl
	                   << rule.to_string();

	// Expand andbit. Warning: after this call the reference on andbit
	// and bitleaf may no longer be valid because the container of
	// and-BITs might have been resorted. so we keep track of their
	// bodies for future use.
	Handle andbit_fcs = andbit.fcs;
	Handle bitleaf_body = bitleaf->body;
	_last_expansion_andbit = _bit.expand(andbit, *bitleaf, {rule, ts}, prob);

	// Record the expansion in the trace atomspace
	if (_last_expansion_andbit) {
		_trace_recorder.andbit(*_last_expansion_andbit);
		_trace_recorder.expansion(andbit_fcs, bitleaf_body,
		                          rule, *_last_expansion_andbit);
	}
}

void BackwardChainer::fulfill_bit()
{
	if (_bit.empty()) {
		ure_logger().warn("Cannot fulfill an empty BIT!");
		return;
	}

	// Select an and-BIT for fulfillment
	const AndBIT* andbit = select_fulfillment_andbit();
	if (andbit == nullptr) {
		ure_logger().debug() << "Cannot fulfill an empty and-BIT. "
		                    << "Abort BIT fulfillment";
		return;
	}
	LAZY_URE_LOG_DEBUG << "Selected and-BIT for fulfillment (fcs value):"
	                   << std::endl << andbit->fcs->id_to_string();

	// Wrap in a try/catch in case the pattern matcher can't handle
	// it.
	try {
		fulfill_fcs(andbit->fcs);
	} catch (...) {}
}

void BackwardChainer::fulfill_fcs(const Handle& fcs)
{
	// Temporary atomspace to not pollute _as with intermediary
	// results
	AtomSpace tmp_as(&_as);

	// Run the FCS and add the results, if any, in _as.
	//
	// Warning: since tmp_as is a child of _as, TVs of existing atoms
	// in _as, that are modified by running fcs will be modified on
	// _as as well. This can create involontary TVs changes, hopefully
	// mitigated by the merging the TVs properly (for now the one with
	// the highest confidence wins). To avoid that side effect, we
	// could operate on a copy the atomspace, of its zone of focus. Or
	// alternatively modify some HypotheticalLink wrapping the atoms
	// of concerns instead of the atoms themselves, and only modify
	// the atoms if there are existing results to copy back to _as.
	Handle hresult = bindlink(&tmp_as, fcs);
	HandleSeq results;
	for (const Handle& result : hresult->getOutgoingSet())
		results.push_back(_as.add_atom(result));
	LAZY_URE_LOG_DEBUG << "Results:" << std::endl << results;
	_results.insert(results.begin(), results.end());

	// Record the results in _trace_as
	for (const Handle& result : results)
		_trace_recorder.proof(fcs, result);
}

std::vector<double> BackwardChainer::expansion_andbit_weights()
{
	std::vector<double> weights;
	for (const AndBIT& andbit : _bit.andbits)
		weights.push_back(operator()(andbit));
	return weights;
}

AndBIT* BackwardChainer::select_expansion_andbit()
{
	std::vector<double> weights = expansion_andbit_weights();

	// Debug log
	if (ure_logger().is_debug_enabled()) {
		OC_ASSERT(weights.size() == _bit.andbits.size());
		std::stringstream ss;
		ss << "Weighted and-BITs:";
		for (size_t i = 0; i < weights.size(); i++)
			ss << std::endl << weights[i] << " "
			   << _bit.andbits[i].fcs->id_to_string();
		ure_logger().debug() << ss.str();
	}

	// Sample andbits according to this distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	return &rand_element(_bit.andbits, dist);
}

const AndBIT* BackwardChainer::select_fulfillment_andbit() const
{
	return _last_expansion_andbit;
}

void BackwardChainer::reduce_bit()
{
	if (0 < _configReader.get_max_bit_size()) {
		// If the BIT size has reached its maximum, randomly remove
		// and-BITs so that the BIT size gets back below or equal to
		// its maximum.
		while (_configReader.get_max_bit_size() < _bit.size()) {
			// Randomly select an and-BIT that is unlikely to be used
			// for the remaining of the inferenceThe and-BITs to remove are
			// selected so that the least likely and-BITs to be
			// selected for expansion are removed first.
			remove_unlikely_expandable_andbit();
		}
	}
}

void BackwardChainer::remove_unlikely_expandable_andbit()
{
	std::vector<double> weights = expansion_andbit_weights();
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	std::vector<double> never_expand_probs;

	// Calculate the probability of never being expanded for the
	// remainder of the inference, thus (1-p) raised to the power of
	// _configReader.get_maximum_iterations() - _iteration. This makes
	// the assumption that the BIT (i.e. its and-BIT population) is
	// not gonna change from this point on, a false but OK assumption
	// for now.
	for (double p : dist.probabilities()) {
		double remaining_iterations =
			_configReader.get_maximum_iterations() - _iteration;
		double nep = std::pow(1 - p, remaining_iterations);
		never_expand_probs.push_back(nep);
	}

	// Fine log
	if (ure_logger().is_fine_enabled()) {
		OC_ASSERT(never_expand_probs.size() == _bit.andbits.size());
		std::stringstream ss;
		ss << "Never expand probs and-BITs:";
		for (size_t i = 0; i < never_expand_probs.size(); i++)
			ss << std::endl << never_expand_probs[i] << " "
			   << _bit.andbits[i].fcs->id_to_string();
		ure_logger().fine() << ss.str();
	}

	std::discrete_distribution<size_t>
		never_expand_dist(never_expand_probs.begin(), never_expand_probs.end());

	// Pick the and-BIT, remove it from the BIT and remove its
	// FCS from the bit atomspace.
	auto it = std::next(_bit.andbits.begin(), never_expand_dist(randGen()));
	LAZY_URE_LOG_DEBUG << "Remove " << it->fcs->id_to_string()
	                   << " from the BIT";
	_bit.erase(it);
}

double BackwardChainer::complexity_factor(const AndBIT& andbit) const
{
	return exp(-_configReader.get_complexity_penalty() * andbit.complexity);
}

double BackwardChainer::operator()(const AndBIT& andbit) const
{
	if (andbit.exhausted)
		return 0.0;
	return _andbit_fitness(andbit) * complexity_factor(andbit);
}
