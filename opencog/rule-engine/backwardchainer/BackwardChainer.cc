/*
 * BackwardChainer.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  October 2014
 *         William Ma <https://github.com/williampma>
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
#include <opencog/atoms/pattern/PatternLink.h>

#include <opencog/query/BindLinkAPI.h>

#include "BackwardChainer.h"
#include "BackwardChainerPMCB.h"
#include "UnifyPMCB.h"
#include "BCLogger.h"

using namespace opencog;

BackwardChainer::BackwardChainer(AtomSpace& as, const Handle& rbs,
                                 const Handle& htarget,
                                 // TODO: add target vardecl
                                 const Handle& hfocus_set, // TODO:
                                                           // support
                                                           // focus_set
                                 const BITFitness& fitness)
	: _as(as), _configReader(as, rbs), _init_target(htarget),
	  _iteration(0), _rules(_configReader.get_rules())
{
	BITNode bit_target(htarget, Handle::UNDEFINED, fitness);
	_handle2bitnode[_init_target] = bit_target;
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

bool BackwardChainer::termination()
{
	return _configReader.get_maximum_iterations() <= _iteration;
}

/**
 * Do a single step of backward chaining.
 */
void BackwardChainer::do_step()
{	
	bc_logger().debug("Iteration %d", _iteration);
	_iteration++;

	// Select target
	BITNode* target = select_target();
	if (not target) {
		bc_logger().warn("No valid target");
		return;
	}
	LAZY_BC_LOG_DEBUG << "Target:" << std::endl << oc_to_string(target);

	// Fulfill target
	fulfill_target(*target);

	// Select a valid rule
	Rule rule = select_rule(*target);
	if (not rule.is_valid()) {
		bc_logger().warn("No valid rule for the selected target");
		return;
	}
	LAZY_BC_LOG_DEBUG << "Rule: " << rule.get_name();

	// Expand the back-inference tree from this target
	expand_bit(*target, rule);
}

BITNode* BackwardChainer::select_target()
{
	if (_handle2bitnode.empty())
		return nullptr;

	// For now selection is uniformly random
	return &(rand_element(_handle2bitnode).second);
}

void BackwardChainer::fulfill_target(BITNode& target)
{
	// TODO
}

Rule BackwardChainer::select_rule(const BITNode& target)
{
	// For now the rule is uniformly randomly selected amongst the
	// valid ones
	RuleSeq valid_rules = get_valid_rules(target);
	if (valid_rules.empty())
		return Rule();
	return rand_element(valid_rules);
}

RuleSeq BackwardChainer::get_valid_rules(const BITNode& target)
{
	RuleSeq valid_rules;
	for (const Rule& rule : _rules) {
		RuleSeq unified_rules = rule.unify_target(target.body, target.vardecl);
		valid_rules.insert(valid_rules.end(),
		                   unified_rules.begin(), unified_rules.end());
	}
	return valid_rules;
}

void BackwardChainer::expand_bit(BITNode& target, const Rule& rule)
{
	// TODO
}

/**
 * Get the current result on the initial target, if any.
 *
 * @return a HandleMultimap mapping each variable to all possible solutions
 */
HandleMultimap BackwardChainer::get_chaining_result()
{
	OC_ASSERT(false, "TODO");
	HandleMultimap temp_result;// = _target_set.get(_init_target).get_varmap();
	HandleMultimap result;
	for (auto& p : temp_result)
	{
		UnorderedHandleSet s;
		for (auto& h : p.second)
			s.insert(_as.get_atom(h));
		result[_as.get_atom(p.first)] = s;
	}

	return result;
}
