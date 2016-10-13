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
#include <opencog/atoms/pattern/BindLink.h>

#include <opencog/query/BindLinkAPI.h>

#include "BackwardChainer.h"
#include "BackwardChainerPMCB.h"
#include "UnifyPMCB.h"
#include "BCLogger.h"

using namespace opencog;

BackwardChainer::BackwardChainer(AtomSpace& as, const Handle& rbs,
                                 const Handle& htarget,
                                 const Handle& vardecl,
                                 const Handle& hfocus_set, // TODO:
                                                           // support
                                                           // focus_set
                                 const BITFitness& fitness)
	: _as(as), _configReader(as, rbs),
	  _init_target(htarget), _init_vardecl(vardecl), _init_fitness(fitness),
	  _iteration(0), _rules(_configReader.get_rules()) {}

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

UREConfigReader& BackwardChainer::get_config()
{
	return _configReader;
}

const UREConfigReader& BackwardChainer::get_config() const
{
	return _configReader;
}

const AndBITFCMap::value_type& BackwardChainer::select_andbit()
{
	// For now selection is uniformly random
	return rand_element(_andbits);
}

BITNode& BackwardChainer::select_bitleaf(const AndBITFCMap::value_type& andbit)
{
	// For now selection is uniformly random
	return _handle2bitnode[rand_element(andbit.first)];
}

BITNode* BackwardChainer::select_target()
{
	if (_handle2bitnode.empty())
		return nullptr;

	// For now selection is uniformly random
	return &(rand_element(_handle2bitnode).second);
}

void BackwardChainer::reduce_bit()
{
	// TODO: avoid having the BIT grow arbitrarily large
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

void BackwardChainer::expand_bit()
{
	if (_handle2bitnode.empty()) {
		// Initialize the and-BIT of the initial target
		insert_h2b(_init_target, _init_vardecl, _init_fitness);
		init_andbits();
	} else {
		// Select an and-BIT and expand it
		const AndBITFCMap::value_type& andbit = select_andbit();
		expand_bit(andbit);
	}
}

void BackwardChainer::expand_bit(const AndBITFCMap::value_type& andbit)
{
	// Select leaf
	BITNode& bitleaf = select_bitleaf(andbit);

	// Select a valid rule
	Rule rule = select_rule(bitleaf);
	if (not rule.is_valid()) {
		bc_logger().warn("No valid rule for the selected bitleaf");
		return;
	}
	LAZY_BC_LOG_DEBUG << "Rule: " << rule.get_name();

	// Expand the back-inference tree from this target
	expand_bit(andbit, bitleaf, rule);
}

void BackwardChainer::expand_bit(const AndBITFCMap::value_type& andbit,
                                 BITNode& leaf, const Rule& rule)
{
	// TODO: support fitness function

	// Expand the leaf
	// 1. Append the rule to it
	// 2. Instantiate the premises as BITNodes
	HandleSeq premises(rule.get_premises());
	leaf.rules.push_back(rule);
	for (const Handle& premise : premises)
		insert_h2b(premise, rule.get_forward_vardecl(), BITFitness());

	// Expand the associated atomese forward chaining strategy
	Handle fcs = expand_fcs(/* TODO */);

	// Define new and-BIT and associate new forward chaining strategy
	// to it
	AndBITFCMap::key_type new_leaves(andbit.first);
	new_leaves.erase(leaf.body);
	new_leaves.insert(premises.begin(), premises.end());
	_andbits[new_leaves] = fcs;
}

Handle BackwardChainer::expand_fcs(/* TODO */)
{
	// TODO
}

void BackwardChainer::fulfill_bit()
{
	if (_andbits.empty()) {
		bc_logger().warn("Cannot fulfill an empty BIT");
		return;
	}

	// Select an and-BIT for fulfillment
	const AndBITFCMap::value_type& andbit = select_andbit();
	fulfill_andbit(andbit);
}

void BackwardChainer::fulfill_andbit(const AndBITFCMap::value_type& andbit)
{
	// TODO
}

void BackwardChainer::insert_h2b(const Handle& body, const Handle& vardecl,
                                 const BITFitness& fitness)
{
	if (body.is_undefined())
		return;

	_handle2bitnode[body] = BITNode(body, vardecl, fitness);
}

void BackwardChainer::init_andbits()
{
	if (_init_target.is_undefined())
		return;

	HandleSeq bl{_init_target, _init_target};
	if (_init_vardecl.is_defined())
		bl.insert(bl.begin(), _init_vardecl);
	Handle fcs = Handle(createBindLink(bl));
	_andbits[{_init_target}] = fcs;
}

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
