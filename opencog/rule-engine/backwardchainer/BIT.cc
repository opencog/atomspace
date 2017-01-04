/*
 * BIT.cc
 *
 * Author: William Ma <https://github.com/williampma>
 *
 * Copyright (C) 2015 OpenCog Foundation
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

#include <boost/range/algorithm/remove_if.hpp>

#include <opencog/util/random.h>

#include <opencog/atomutils/Neighbors.h>
#include "BIT.h"
#include "BCLogger.h"

namespace opencog {

BITNode::BITNode(const Handle& bd, const BITFitness& fit)
	: body(bd), fitness(fit) {}

std::string	BITNode::to_string() const
{
	std::stringstream ss;
	ss << "body:" << std::endl << oc_to_string(body)
	   << "rules:" << std::endl << oc_to_string(rules);
	return ss.str();
}

BIT::BIT(AtomSpace& as,
         const Handle& target,
         const Handle& vardecl,
         const BITFitness& fitness)
	: bit_as(&as),
	  _init_target(target), _init_vardecl(vardecl), _init_fitness(fitness)
{}

bool BIT::empty() const
{
	return _handle2bitnode.empty();
}

Handle BIT::init()
{
	// Initialize the BIT with an FCS basedon the initial target
	init_fcss();

	LAZY_BC_LOG_DEBUG << "Initialize BIT with:" << std::endl
	                  << _handle2bitnode[_init_target].to_string();

	return *_fcss.begin();
}

Handle BIT::expand(const Handle& fcs, BITNode& bitleaf, const Rule& rule)
{
	// Make sure that the rule is not already an or-child of bitleaf.
	if (is_in(rule, bitleaf)) {
		bc_logger().debug() << "An equivalent rule has already expanded "
		                    << "that BIT-node, abort expansion";
		return Handle::UNDEFINED;
	}

	// Insert the rule as or-branch of this bitleaf
	bitleaf.rules.insert(rule);

	// Expand the FCS
 	Handle new_fcs = expand_fcs(fcs, bitleaf.body, rule);

	// Insert the new FCS into the BIT
	insert_fcs(new_fcs);

	return new_fcs;
}

Handle BIT::select_fcs() const
{
	return rand_element(_fcss);
}

BITNode& BIT::select_bitleaf(const Handle& fcs)
{
	// For now selection is uniformly random
	return _handle2bitnode[rand_element(get_leaves(fcs))];
}

void BIT::insert_bitnode(Handle body, Handle vardecl, const BITFitness& fitness)
{
	if (body.is_undefined())
		return;

	if (_handle2bitnode.find(body) == _handle2bitnode.end())
		_handle2bitnode[body] = BITNode(body, fitness);
}

void BIT::init_fcss()
{
	if (_init_target.is_undefined())
		return;

	// Create initial FCS
	HandleSeq bl{_init_target, _init_target};
	if (_init_vardecl.is_defined())
		bl.insert(bl.begin(), _init_vardecl);
	Handle fcs = bit_as.add_link(BIND_LINK, bl);

	// Insert it into the BIT
	insert_fcs(fcs);
}

void BIT::insert_fcs(const Handle& fcs)
{
	auto it = _fcss.find(fcs);
	if (it == _fcss.end()) {
		// Check that it is not alpha-equivalent either
		for (const Handle& efcs : _fcss) {
			if (ScopeLinkCast(fcs)->is_equal(efcs)) {
				LAZY_BC_LOG_DEBUG << "The following FCS is alpha-equivalent "
				                  << "to another one already in the BIT:"
				                  << std::endl << oc_to_string(fcs);
				return;
			}
		}

		_fcss.insert(fcs);

		// For each leave associate a corresponding BITNode
		Handle fcs_vardecl = BindLinkCast(fcs)->get_vardecl();
		for (const Handle& leaf : get_leaves(fcs)) {
			Handle leaf_vardecl = filter_vardecl(fcs_vardecl, leaf);
			insert_bitnode(leaf, leaf_vardecl, BITFitness());
		}
	}
	else {
		LAZY_BC_LOG_DEBUG << "The following FCS is already in the BIT:"
		                  << std::endl << oc_to_string(fcs);
	}
}

bool BIT::is_in(const Rule& rule, const BITNode& bitnode)
{
	for (const Rule& bnr : bitnode.rules)
		if (rule.is_alpha_equivalent(bnr))
			return true;
	return false;
}

OrderedHandleSet BIT::get_leaves(const Handle& h) const
{
	Type t = h->getType();
	if (t == BIND_LINK) {
		BindLinkPtr hsc = BindLinkCast(h);
		Handle rewrite = hsc->get_implicand();
		return get_leaves(rewrite);
	} else if (t == EXECUTION_OUTPUT_LINK) {
		// All arguments except the last one are potential target leaves
		Handle args = h->getOutgoingAtom(1);
		OrderedHandleSet leaves;
		if (args->getType() == LIST_LINK) {
			OC_ASSERT(args->getArity() > 0);
			for (Arity i = 0; i+1 < args->getArity(); i++) {
				OrderedHandleSet aleaves = get_leaves(args->getOutgoingAtom(i));
				leaves.insert(aleaves.begin(), aleaves.end());
			}
		} else
			leaves.insert(args);
		return leaves;
	} else if (t == SET_LINK) {
		// All atoms wrapped in a SetLink are potential target leaves
		OrderedHandleSet leaves;
		for (const Handle& el : h->getOutgoingSet()) {
			OrderedHandleSet el_leaves = get_leaves(el);
			leaves.insert(el_leaves.begin(), el_leaves.end());
		}
		return leaves;
	} else
		// It is probably a leaf so return it
		return OrderedHandleSet{h};
}

Handle BIT::expand_fcs(const Handle& fcs, const Handle& leaf, const Rule& rule)
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
	Handle nvardecl = filter_vardecl(merge_vardecl(nfcs_vardecl, rule_vardecl),
	                                 {npattern, nrewrite});

	// Generate new atomese forward chaining strategy
	HandleSeq noutgoings({npattern, nrewrite});
	if (nvardecl.is_defined())
		noutgoings.insert(noutgoings.begin(), nvardecl);
	nfcs = bit_as.add_link(BIND_LINK, noutgoings);

	LAZY_BC_LOG_DEBUG << "Expanded forward chainer strategy:" << std::endl
	                  << "from:" << std::endl << fcs
	                  << "to:" << std::endl << nfcs;

	return nfcs;
}

Handle BIT::substitute_unified_variables(const Handle& fcs,
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
	Handle leaf_vardecl = filter_vardecl(fcs_bl->get_vardecl(), leaf),
		conclusion_vardecl = rule.get_forward_vardecl();
	bc_logger().debug() << "BIT::substitute_unified_variables "
	                    << "leaf = " << oc_to_string(leaf)
	                    << "conclusion = " << oc_to_string(conclusion)
	                    << "leaf_vardecl = " << oc_to_string(leaf_vardecl)
	                    << "conclusion_vardecl = " << oc_to_string(conclusion_vardecl);
	UnificationSolutionSet sol =
		unify(leaf, conclusion, leaf_vardecl, conclusion_vardecl);

	bc_logger().debug() << "BIT::substitute_unified_variables sol: "
	                    << oc_to_string(sol);

	OC_ASSERT(sol.satisfiable); // If the rule has been selected it
                                // has to be satisfiable
	TypedSubstitutions tss =
		typed_substitutions(sol, leaf, leaf, conclusion,
		                    fcs_bl->get_vardecl(), conclusion_vardecl);
	OC_ASSERT(not tss.empty());
	auto ts = *tss.begin();
	return Handle(substitute(fcs_bl, ts));
}

Handle BIT::expand_fcs_pattern(const Handle& fcs_pattern, const Rule& rule)
{
	HandleSeq clauses = rule.get_premises();
	HandlePairSeq conclusions = rule.get_conclusions();
	OC_ASSERT(conclusions.size() == 1);
	Handle conclusion = conclusions[0].second;

	// Simple case where the fcs contains the conclusion itself
	if (content_eq(fcs_pattern, conclusion))
		return bit_as.add_link(AND_LINK, HandleSeq(clauses.begin(),
		                                           clauses.end()));

	// The fcs contains a conjunction of clauses. Remove any clause
	// that is equal to the rule conclusion, or precondition that uses
	// that conclusion as argument.
	OC_ASSERT(fcs_pattern->getType() == AND_LINK);
	HandleSeq fcs_clauses = fcs_pattern->getOutgoingSet();
	auto to_remove = [&](const Handle& h) {
		return (is_locally_quoted_eq(h, conclusion)
		        or is_argument_of(h, conclusion));
	};
	fcs_clauses.erase(boost::remove_if(fcs_clauses, to_remove),
	                  fcs_clauses.end());

	// Add the patterns and preconditions associated to the rule
	fcs_clauses.insert(fcs_clauses.end(), clauses.begin(), clauses.end());

	return bit_as.add_link(AND_LINK, fcs_clauses);
}

Handle BIT::expand_fcs_rewrite(const Handle& fcs_rewrite, const Rule& rule)
{
	HandlePairSeq conclusions = rule.get_conclusions();
	OC_ASSERT(conclusions.size() == 1);
	Handle conclusion = conclusions[0].second;

	// Base cases

	// Replace the fcs rewrite atoms by the rule rewrite if equal to
	// the rule conclusion
	if (content_eq(fcs_rewrite, conclusion))
		return rule.get_forward_implicand();
	// If node and isn't equal to conclusion leave alone
	if (fcs_rewrite->isNode())
		return fcs_rewrite;

	// Recursive case

	Type t = fcs_rewrite->getType();
	HandleSeq outgoings;
	for (const Handle& h : fcs_rewrite->getOutgoingSet())
		outgoings.push_back(expand_fcs_rewrite(h, rule));
	return bit_as.add_link(t, outgoings);
}

bool BIT::is_argument_of(const Handle& eval, const Handle& atom)
{
	if (eval->getType() == EVALUATION_LINK) {
		Handle args = eval->getOutgoingAtom(1);
		if (content_eq(args, atom))
			return true;
		if (args->getType() == LIST_LINK)
			for (Arity i = 0; i+1 < args->getArity(); i++)
				if (content_eq(args->getOutgoingAtom(i), atom))
					return true;
	}
	return false;
}

bool BIT::is_locally_quoted_eq(const Handle& lhs, const Handle& rhs)
{
	if (content_eq(lhs, rhs))
		return true;
	Type lhs_t = lhs->getType();
	Type rhs_t = rhs->getType();
	if (lhs_t == LOCAL_QUOTE_LINK and rhs_t != LOCAL_QUOTE_LINK)
		return content_eq(lhs->getOutgoingAtom(0), rhs);
	if (lhs_t != LOCAL_QUOTE_LINK and rhs_t == LOCAL_QUOTE_LINK)
		return content_eq(lhs, rhs->getOutgoingAtom(0));
	return false;
}

std::string oc_to_string(const BITNode& bitnode)
{
	return bitnode.to_string();
}

std::string oc_to_string(const HandleBITNodeMap& hbn)
{
	std::stringstream ss;
	ss << "size = " << hbn.size() << std::endl;
	size_t i = 0;
	for (const auto& el : hbn) {
		ss << "Handle[" << i << "]:" << std::endl
		   << oc_to_string(el.first)
		   << "BITNode[" << i << "]:" << std::endl
		   << oc_to_string(el.second);
	}
	return ss.str();
}

} // ~namespace opencog
