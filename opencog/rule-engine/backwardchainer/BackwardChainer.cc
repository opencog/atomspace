/*
 * BackwardChainer.cc
 *
 * Copyright (C) 2014-2016 OpenCog Foundation
 *
 * Authors: Misgana Bayetta <misgana.bayetta@gmail.com>  October 2014
 *          William Ma <https://github.com/williampma>
 *          Nil Geisweiller 2016
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
                                 const BITFitness& fitness)
	: _as(as), _configReader(as, rbs), _bit(as, target, vardecl, fitness),
	  _iteration(0), _last_expansion_fcs(Handle::UNDEFINED),
	  _rules(_configReader.get_rules()) {}

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
	_rules.expand_meta_rules(_as);

	// Reset _last_expansion_fcs
	_last_expansion_fcs = Handle::UNDEFINED;

	if (_bit.empty()) {
		_last_expansion_fcs = _bit.init();
	} else {
		// Select an FCS (i.e. and-BIT) and expand it
		Handle fcs = select_expansion_fcs();
		LAZY_BC_LOG_DEBUG << "Selected FCS for expansion:" << std::endl
		                  << fcs;

		// Hack before we code a proper complexity penalty to avoid
		// exploring overly complex and-BITs.
		if (fcs->size() > 1000) {
			bc_logger().debug() << "The selected FCS size it too big, abort expansion";
			return;
		}
		expand_bit(fcs);
	}
}

void BackwardChainer::expand_bit(const Handle& fcs)
{
	// Select leaf
	BITNode& bitleaf = _bit.select_bitleaf(fcs);
	LAZY_BC_LOG_DEBUG << "Selected BIT-node for expansion:" << std::endl
	                  << bitleaf.to_string();

	// Build the leaf vardecl from fcs
	Handle vardecl = fcs.is_defined() ?
		filter_vardecl(BindLinkCast(fcs)->get_vardecl(), bitleaf.body)
		: Handle::UNDEFINED;

	// Select a valid rule
	Rule rule = select_rule(bitleaf, vardecl);
	// Add the rule in the _bit.bit_as to make comparing atoms easier
	rule.add(_bit.bit_as);
	if (not rule.is_valid()) {
		bc_logger().debug("No valid rule for the selected BIT-node, abort expansion");
		return;
	}
	LAZY_BC_LOG_DEBUG << "Selected rule for BIT expansion:" << std::endl
	                  << rule.to_string();

	// Expand the back-inference tree from this target
	_last_expansion_fcs = _bit.expand(fcs, bitleaf, rule);
}

void BackwardChainer::fulfill_bit()
{
	if (_bit.empty()) {
		bc_logger().warn("Cannot fulfill an empty BIT!");
		return;
	}

	// Select an and-BIT for fulfillment
	Handle fcs = select_fulfillment_fcs();
	if (fcs == Handle::UNDEFINED) {
		bc_logger().debug() << "Cannot fulfill an empty FCS. Abort BIT fulfillment";
		return;
	}
	LAZY_BC_LOG_DEBUG << "Selected FCS for fulfillment:" << std::endl
	                  << fcs;
	fulfill_fcs(fcs);
}

void BackwardChainer::fulfill_fcs(const Handle& fcs)
{
	Handle hresult = bindlink(&_as, fcs);
	const HandleSeq& results = hresult->getOutgoingSet();
	LAZY_BC_LOG_DEBUG << "Results:" << std::endl << results;
	_results.insert(results.begin(), results.end());
}

Handle BackwardChainer::select_expansion_fcs() const
{
	return _bit.select_fcs();
}

Handle BackwardChainer::select_fulfillment_fcs() const
{
	// Select the lastly expanded and-BIT
	if (_last_expansion_fcs.is_defined())
		return _last_expansion_fcs;

	// If the last expansion has failed then don't waste time retry to
	// fulfill an old and-BIT
	return Handle::UNDEFINED;
}

void BackwardChainer::reduce_bit()
{
	// TODO: avoid having the BIT grow arbitrarily large
}

Rule BackwardChainer::select_rule(const BITNode& target, const Handle& vardecl)
{
	// For now the rule is uniformly randomly selected amongst the
	// valid ones
	const RuleSet valid_rules = get_valid_rules(target, vardecl);
	if (valid_rules.empty())
		return Rule();

	// Log all valid rules
	if (bc_logger().is_debug_enabled()) {
		std::stringstream ss;
		ss << "The following rules are valid:";
		for (const Rule& r : valid_rules)
			ss << std::endl << r.get_name();
		LAZY_BC_LOG_DEBUG << ss.str();
	}

	return rand_element(valid_rules);
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
		valid_rules.insert(unified_rules.begin(), unified_rules.end());
	}
	return valid_rules;
}
