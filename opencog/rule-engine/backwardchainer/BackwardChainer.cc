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

#include <boost/range/algorithm/find_if.hpp>

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
                                 const Handle& htarget,
                                 const Handle& vardecl,
                                 const Handle& hfocus_set, // TODO:
                                                           // support
                                                           // focus_set
                                 const BITFitness& fitness)
	: _as(as), _configReader(as, rbs),
	  _init_target(htarget), _init_vardecl(vardecl), _init_fitness(fitness),
	  _bit_as(&as), _iteration(0), _rules(_configReader.get_rules()) {}

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

	if (_handle2bitnode.empty()) {
		// Initialize the and-BIT of the initial target
		insert_h2b(_init_target, _init_vardecl, _init_fitness);
		init_andbits();

		LAZY_BC_LOG_DEBUG << "Initialize BIT with:" << std::endl
		                  << _handle2bitnode[_init_target].to_string();
	} else {
		// Select an and-BIT and expand it
		const AndBITFCMap::value_type& andbit = select_andbit();
		LAZY_BC_LOG_DEBUG << "Selected and-BIT for expansion:" << std::endl
		                  << andbit.second;
		expand_bit(andbit);
	}
}

void BackwardChainer::expand_bit(const AndBITFCMap::value_type& andbit)
{
	// Select leaf
	BITNode& bitleaf = select_bitleaf(andbit);
	LAZY_BC_LOG_DEBUG << "Selected BIT-node for expansion:" << std::endl
	                  << bitleaf.to_string();

	// Select a valid rule
	Rule rule = select_rule(bitleaf);
	rule.add(_bit_as);
	if (not rule.is_valid()) {
		bc_logger().warn("No valid rule for the selected BIT-node, abort expansion");
		return;
	}
	LAZY_BC_LOG_DEBUG << "Selected rule for BIT expansion:" << std::endl
	                  << rule.to_string();

	// Expand the back-inference tree from this target
	expand_bit(andbit, bitleaf, rule);
}

void BackwardChainer::expand_bit(const AndBITFCMap::value_type& andbit,
                                 BITNode& bitleaf, const Rule& rule)
{
	// Make sure that the rule is not already an or-child of bitleaf.
	if (is_in(rule, bitleaf)) {
		bc_logger().debug() << "An equivalent rule has already expanded "
		                    << "that BIT-node, abort expansion";
		return;
	}

	// Expand the associated atomese forward chaining strategy
 	Handle fcs = expand_fcs(andbit.second, bitleaf.body, rule);

	// Create the new and-BIT, insert it into the BIT and associate it
	// to fcs
	expand_bit(fcs);
}

OrderedHandleSet BackwardChainer::get_fcs_leaves(const Handle& fcs)
{
	Handle fcs_pattern = BindLinkCast(fcs)->get_body();
	OrderedHandleSet leaves;
	if (fcs_pattern->getType() == AND_LINK) {
		const HandleSeq& outgoings = fcs_pattern->getOutgoingSet();
		leaves.insert(outgoings.begin(), outgoings.end());
	} else {
		leaves.insert(fcs_pattern);
	}
	return leaves;
}

void BackwardChainer::expand_bit(const Handle& fcs)
{
	// Associate the fcs leaves (which defines an and-BIT) to itself
	OrderedHandleSet leaves = get_fcs_leaves(fcs);
	_andbits[leaves] = fcs;

	// For each leave associate a corresponding BITNode
	Handle fcs_vardecl = BindLinkCast(fcs)->get_vardecl();
	for (const Handle& leaf : leaves)
		insert_h2b(leaf, fcs_vardecl, BITFitness());
}

Handle BackwardChainer::expand_fcs(const Handle& fcs, const Handle& leaf,
                                   const Rule& rule)
{
	// Unify the rule conclusion with the leaf, and substitute any
	// variables in it by the associated term.
	Handle nfcs = substitute_unified_variables(fcs, leaf, rule);

	BindLinkPtr nfcs_bl(BindLinkCast(nfcs));
	Handle nfcs_vardecl = nfcs_bl->get_vardecl();
	Handle nfcs_pattern = nfcs_bl->get_body();
	Handle nfcs_rewrite = nfcs_bl->get_implicand();
	Handle rule_vardecl = rule.get_forward_vardecl();

	// Generate new pattern term
	Handle npattern = expand_fcs_pattern(nfcs_pattern, rule);

	// Generate new rewrite term
	Handle nrewrite = expand_fcs_rewrite(nfcs_rewrite, rule);

	// Generate new vardecl
	// TODO: revisit to which extend this is necessary
	Handle nvardecl = filter_vardecl(merge_vardecl(nfcs_vardecl, rule_vardecl),
	                                 {npattern, nrewrite});

	// Generate new atomese forward chaining strategy
	HandleSeq noutgoings({npattern, nrewrite});
	if (nvardecl.is_defined())
		noutgoings.insert(noutgoings.begin(), nvardecl);
	nfcs = _bit_as.add_link(BIND_LINK, noutgoings);

	LAZY_BC_LOG_DEBUG << "Expanded forward chainer strategy:" << std::endl << fcs
	                  << "to:" << std::endl << nfcs;

	return nfcs;
}

Handle BackwardChainer::substitute_unified_variables(const Handle& fcs,
                                                     const Handle& leaf,
                                                     const Rule& rule)
{
	HandlePairSeq conclusions = rule.get_conclusions();
	OC_ASSERT(conclusions.size() == 1);
	Handle conclusion = conclusions[0].second;

	// If the conclusion is already equal to leaf no need to unify
	if (content_eq(conclusion, leaf))
		return fcs;

	BindLinkPtr fcs_bl(BindLinkCast(fcs));
	UnificationSolutionSet sol =
		unify(leaf, conclusion,
		      fcs_bl->get_vardecl(), rule.get_forward_vardecl());

	OC_ASSERT(sol.satisfiable); // If the rule has been selected it
                                // has to be satisfiable
	TypedSubstitutions tss = typed_substitutions(sol, leaf);
	OC_ASSERT(not tss.empty());
	auto ts = *tss.begin();
	HandleSeq values = fcs_bl->get_variables().make_values(ts.first);
	return fcs_bl->alpha_conversion(values, ts.second);
}

Handle BackwardChainer::expand_fcs_pattern(const Handle& fcs_pattern,
                                           const Rule& rule)
{
	HandleSeq premises = rule.get_premises();
	HandlePairSeq conclusions = rule.get_conclusions();
	OC_ASSERT(conclusions.size() == 1);
	Handle conclusion = conclusions[0].second;

	if (fcs_pattern == conclusion)
		return _bit_as.add_link(AND_LINK, premises);

	OC_ASSERT(fcs_pattern->getType() == AND_LINK);
	HandleSeq outgoings = fcs_pattern->getOutgoingSet();
	auto equal_to_conclusion = [&](const Handle& h) {
		return content_eq(h, conclusion);
	};
	// TODO: since AndLink is ordered, we might speed that up by using
	// std::lower_bound
	auto it = boost::find_if(outgoings, equal_to_conclusion);
	OC_ASSERT(it != outgoings.end());
	outgoings.erase(it);
	outgoings.insert(outgoings.end(), premises.begin(), premises.end());
	return _bit_as.add_link(AND_LINK, outgoings);
}

Handle BackwardChainer::expand_fcs_rewrite(const Handle& fcs_rewrite,
                                           const Rule& rule)
{
	HandlePairSeq conclusions = rule.get_conclusions();
	OC_ASSERT(conclusions.size() == 1);
	Handle conclusion = conclusions[0].second;

	// Base cases

	// Replace the fcs rewrite atoms by the rule rewrite if equal to
	// the rule conclusion
	if (fcs_rewrite == conclusion)
		return rule.get_forward_implicand();
	// If node and isn't equal to conclusion leave alone
	if (fcs_rewrite->isNode())
		return fcs_rewrite;

	// Recursive case

	Type t = fcs_rewrite->getType();
	HandleSeq outgoings;
	for (const Handle& h : fcs_rewrite->getOutgoingSet())
		outgoings.push_back(expand_fcs_rewrite(h, rule));
	return _bit_as.add_link(t, outgoings);
}

void BackwardChainer::fulfill_bit()
{
	if (_andbits.empty()) {
		bc_logger().warn("Cannot fulfill an empty BIT");
		return;
	}

	// Select an and-BIT for fulfillment
	const AndBITFCMap::value_type& andbit = select_andbit();
	LAZY_BC_LOG_DEBUG << "Selected and-BIT for fulfillment:" << std::endl
	                  << andbit.second;
	fulfill_andbit(andbit);
}

void BackwardChainer::fulfill_andbit(const AndBITFCMap::value_type& andbit)
{
	Handle hresult = bindlink(&_as, andbit.second);
	const HandleSeq& results = hresult->getOutgoingSet();
	LAZY_BC_LOG_DEBUG << "Results:" << std::endl << results;
	_results.insert(results.begin(), results.end());
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

void BackwardChainer::reduce_bit()
{
	// TODO: avoid having the BIT grow arbitrarily large
}

Rule BackwardChainer::select_rule(const BITNode& target)
{
	// For now the rule is uniformly randomly selected amongst the
	// valid ones
	const RuleSet valid_rules = get_valid_rules(target);
	if (valid_rules.empty())
		return Rule();
	return rand_element(valid_rules);
}

RuleSet BackwardChainer::get_valid_rules(const BITNode& target)
{
	RuleSet valid_rules;
	for (const Rule& rule : _rules) {
		RuleSet unified_rules = rule.unify_target(target.body, target.vardecl);
		valid_rules.insert(unified_rules.begin(), unified_rules.end());
	}
	return valid_rules;
}

void BackwardChainer::insert_h2b(Handle body, Handle vardecl,
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
	Handle fcs = _bit_as.add_link(BIND_LINK, bl);
	_andbits[{_init_target}] = fcs;
}

bool BackwardChainer::is_in(const Rule& rule, const BITNode& bitnode)
{
	for (const Rule& bnr : bitnode.rules)
		if (rule.is_alpha_equivalent(bnr))
			return true;
	return false;
}
