/*
 * BIT.cc
 *
 * Copyright (C) 2016-2017 OpenCog Foundation
 * Author: Nil Geisweiller
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

#include <boost/range/algorithm/binary_search.hpp>
#include <boost/range/algorithm/lower_bound.hpp>
#include <boost/range/algorithm/remove_if.hpp>
#include <boost/range/algorithm/unique.hpp>
#include <boost/range/algorithm/find.hpp>
#include <boost/range/algorithm/sort.hpp>

#include <opencog/util/random.h>

#include <opencog/atomutils/Neighbors.h>
#include <opencog/atomutils/FindUtils.h>

#include "BIT.h"
#include "BCLogger.h"

namespace opencog {

/////////////
// BITNode //
/////////////

BITNode::BITNode(const Handle& bd, const BITNodeFitness& fi)
	: body(bd), fitness(fi), exhausted(false) {}

double BITNode::operator()() const
{
	// The probably is anti-proportional to the fitness. Indeed if the
	// fitness already is high, expanding the BIT-Node won't likely
	// add anything to the success of the inference.
	return (exhausted ? 0.0 : 1.0) *
		(fitness.upper - fitness(*this)) / (fitness.upper - fitness.lower);
}

std::string	BITNode::to_string() const
{
	std::stringstream ss;
	ss << "body:" << std::endl << oc_to_string(body)
	   << "exhausted: " << exhausted << std::endl
	   << "rules: " << oc_to_string(rules);
	return ss.str();
}

////////////
// AndBIT //
////////////

AndBIT::AndBIT() : exhausted(false) {}

AndBIT::AndBIT(AtomSpace& as, const Handle& target, const Handle& vardecl,
               const BITNodeFitness& fitness) : exhausted(false)
{
	// Create initial FCS
	HandleSeq bl{target, target};
	if (vardecl.is_defined())
		bl.insert(bl.begin(), vardecl);
	fcs = as.add_link(BIND_LINK, bl);

	insert_bitnode(target, fitness);
}

AndBIT::AndBIT(const Handle& f) : fcs(f), exhausted(false)
{
	set_leaf2bitnode();         // TODO: might differ till needed
}

AndBIT::~AndBIT() {}

AndBIT AndBIT::expand(const Handle& leaf, const Rule& rule) const
{
	return AndBIT(expand_fcs(leaf, rule));
}

BITNode* AndBIT::select_leaf()
{
	// Generate the distribution over target leaves according to the
	// BIT-node fitnesses. The higher the fitness the lower the chance
	// of being selected as it is already fit.
	std::vector<double> weights;
	bool all_weights_null = true;
	for (const auto& lb : leaf2bitnode) {
		double p = lb.second();
		weights.push_back(p);
		if (p > 0) all_weights_null = false;
	}

	// Check that the distribution is well defined
	if (all_weights_null)
		return nullptr;

	// If well defined then sample according to it
	LeafDistribution dist(weights.begin(), weights.end());
	return &rand_element(leaf2bitnode, dist).second;
}

void AndBIT::reset_exhausted()
{
	for (auto& el : leaf2bitnode)
		el.second.exhausted = false;
	exhausted = false;
}

bool AndBIT::operator==(const AndBIT& andbit) const
{
	return fcs == andbit.fcs;
}

bool AndBIT::operator<(const AndBIT& andbit) const
{
	size_t fcs_size = fcs->size();
	size_t other_size = andbit.fcs->size();
	return (fcs_size < other_size)
		or (fcs_size == other_size and fcs < andbit.fcs);
}

std::string AndBIT::to_string() const
{
	return oc_to_string(fcs);
}

Handle AndBIT::expand_fcs(const Handle& leaf, const Rule& rule) const
{
	// Unify the rule conclusion with the leaf, and substitute any
	// variables in it by the associated term.
	Handle nfcs = substitute_unified_variables(leaf, rule);

	BindLinkPtr nfcs_bl(BindLinkCast(nfcs));
	Handle nfcs_vardecl = nfcs_bl->get_vardecl();
	Handle nfcs_pattern = nfcs_bl->get_body();
	Handle nfcs_rewrite = nfcs_bl->get_implicand();
	Handle rule_vardecl = rule.get_vardecl();

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
	nfcs = fcs->getAtomSpace()->add_link(BIND_LINK, noutgoings);

	LAZY_BC_LOG_DEBUG << "Expanded forward chainer strategy:" << std::endl
	                  << "from:" << std::endl << fcs
	                  << "to:" << std::endl << nfcs;

	return nfcs;
}

void AndBIT::set_leaf2bitnode()
{
	// For each leaf of fcs, associate a corresponding BITNode
	for (const Handle& leaf : get_leaves())
		insert_bitnode(leaf, BITNodeFitness());
}

void AndBIT::insert_bitnode(Handle leaf, const BITNodeFitness& fitness)
{
	if (leaf.is_undefined())
		return;

	if (leaf2bitnode.find(leaf) == leaf2bitnode.end())
		leaf2bitnode.emplace(leaf, BITNode(leaf, fitness));
}

OrderedHandleSet AndBIT::get_leaves() const
{
	return get_leaves(fcs);
}

OrderedHandleSet AndBIT::get_leaves(const Handle& h) const
{
	Type t = h->getType();
	if (t == BIND_LINK) {
		BindLinkPtr hsc = BindLinkCast(h);
		Handle rewrite = hsc->get_implicand();
		return get_leaves(rewrite);
	} else if (t == EXECUTION_OUTPUT_LINK) {
		// All arguments except the first one are potential target leaves
		Handle args = h->getOutgoingAtom(1);
		OrderedHandleSet leaves;
		if (args->getType() == LIST_LINK) {
			OC_ASSERT(args->getArity() > 0);
			for (Arity i = 1; i < args->getArity(); i++) {
				OrderedHandleSet aleaves = get_leaves(args->getOutgoingAtom(i));
				leaves.insert(aleaves.begin(), aleaves.end());
			}
		}
		return leaves;
	} else if (t == SET_LINK) {
		// All atoms wrapped in a SetLink are potential target leaves
		OrderedHandleSet leaves;
		for (const Handle& el : h->getOutgoingSet()) {
			OrderedHandleSet el_leaves = get_leaves(el);
			leaves.insert(el_leaves.begin(), el_leaves.end());
		}
		return leaves;
	} else if (contains_atomtype(h, EXECUTION_OUTPUT_LINK)) {
		// If it contains an unquoted ExecutionOutputLink then it is
		// not a leaf (maybe it could but it would over complicate the
		// rest and bring no benefit since we can always expand a
		// parent and-BIT that has no such ExecutionOutputLink).
		return OrderedHandleSet{};
	}

	// Here it must be a leaf so return it
	return OrderedHandleSet{h};
}

Handle AndBIT::substitute_unified_variables(const Handle& leaf,
                                            const Rule& rule) const
{
	HandlePairSeq conclusions = rule.get_conclusions();
	OC_ASSERT(conclusions.size() == 1);
	Handle conclusion = conclusions[0].second;

	// If the conclusion is already equal to leaf no need to unify
	if (content_eq(conclusion, leaf))
		return fcs;

	BindLinkPtr fcs_bl(BindLinkCast(fcs));
	Handle leaf_vardecl = filter_vardecl(fcs_bl->get_vardecl(), leaf),
		conclusion_vardecl = rule.get_vardecl();
	UnificationSolutionSet sol =
		unify(leaf, conclusion, leaf_vardecl, conclusion_vardecl);

	OC_ASSERT(sol.satisfiable); // If the rule has been selected it
                                // has to be satisfiable
	TypedSubstitutions tss =
		typed_substitutions(sol, leaf, leaf, conclusion,
		                    fcs_bl->get_vardecl(), conclusion_vardecl);
	OC_ASSERT(not tss.empty());
	auto ts = *tss.begin();
	return Handle(substitute(fcs_bl, ts));
}

Handle AndBIT::expand_fcs_pattern(const Handle& fcs_pattern,
                                  const Rule& rule) const
{
	HandleSeq clauses = rule.get_clauses();
	HandlePairSeq conclusions = rule.get_conclusions();
	OC_ASSERT(conclusions.size() == 1);
	Handle conclusion = conclusions[0].second;

	// Simple case where the fcs contains the conclusion itself
	if (content_eq(fcs_pattern, conclusion))
		return fcs->getAtomSpace()->add_link(AND_LINK,
		                                     HandleSeq(clauses.begin(),
		                                               clauses.end()));

	// The fcs contains a conjunction of clauses
	OC_ASSERT(fcs_pattern->getType() == AND_LINK);

	// Remove any fcs clause that:
	//
	// 1. is equal to the rule conclusion.
	//
	// 2. is a precondition that uses that conclusion as argument.
	HandleSeq fcs_clauses = fcs_pattern->getOutgoingSet();
	auto to_remove = [&](const Handle& h) {
		return (is_locally_quoted_eq(h, conclusion)
		        or is_argument_of(h, conclusion)
		        or (boost::find(clauses, h) != clauses.end()));
	};
	fcs_clauses.erase(boost::remove_if(fcs_clauses, to_remove),
	                  fcs_clauses.end());

	// Add the patterns and preconditions associated to the rule
	fcs_clauses.insert(fcs_clauses.end(), clauses.begin(), clauses.end());

	// Remove redundant clauses
	boost::unique(boost::sort(fcs_clauses));

	return fcs->getAtomSpace()->add_link(AND_LINK, fcs_clauses);
}

Handle AndBIT::expand_fcs_rewrite(const Handle& fcs_rewrite,
                                  const Rule& rule) const
{
	HandlePairSeq conclusions = rule.get_conclusions();
	OC_ASSERT(conclusions.size() == 1);
	Handle conclusion = conclusions[0].second;

	// Base cases

	// Replace the fcs rewrite atoms by the rule rewrite if equal to
	// the rule conclusion
	if (content_eq(fcs_rewrite, conclusion))
		return rule.get_implicand();
	// If node and isn't equal to conclusion leave alone
	if (fcs_rewrite->isNode())
		return fcs_rewrite;

	// Recursive case

	Type t = fcs_rewrite->getType();
	HandleSeq outgoings;
	for (const Handle& h : fcs_rewrite->getOutgoingSet())
		outgoings.push_back(expand_fcs_rewrite(h, rule));
	return fcs->getAtomSpace()->add_link(t, outgoings);
}

bool AndBIT::is_argument_of(const Handle& eval, const Handle& atom) const
{
	if (eval->getType() == EVALUATION_LINK) {
		Handle args = eval->getOutgoingAtom(1);
		if (content_eq(args, atom))
			return true;
		if (args->getType() == LIST_LINK)
			for (Arity i = 0; i < args->getArity(); i++)
				if (content_eq(args->getOutgoingAtom(i), atom))
					return true;
	}
	return false;
}

bool AndBIT::is_locally_quoted_eq(const Handle& lhs, const Handle& rhs) const
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

/////////
// BIT //
/////////

BIT::BIT(AtomSpace& as,
         const Handle& target,
         const Handle& vardecl,
         const BITNodeFitness& fitness)
	: bit_as(&as),
	  _init_target(target), _init_vardecl(vardecl), _init_fitness(fitness)
{}

BIT::~BIT() {}

bool BIT::empty() const
{
	return andbits.empty();
}

AndBIT* BIT::init()
{
	andbits.emplace_back(bit_as, _init_target, _init_vardecl, _init_fitness);

	LAZY_BC_LOG_DEBUG << "Initialize BIT with:" << std::endl
	                  << andbits.begin()->to_string();

	return &*andbits.begin();
}

AndBIT* BIT::expand(AndBIT& andbit, BITNode& bitleaf, const Rule& rule)
{
	// Make sure that the rule is not already an or-child of bitleaf.
	if (is_in(rule, bitleaf)) {
		bc_logger().debug() << "An equivalent rule has already expanded "
		                    << "that BIT-node, abort expansion";
		return nullptr;
	}

	// Insert the rule as or-branch of this bitleaf
	bitleaf.rules.insert(rule);

	// Expand the and-BIT and insert it in the BIT
	return insert(andbit.expand(bitleaf.body, rule));
}

AndBIT* BIT::insert(const AndBIT& andbit)
{
	// Check that it isn't already in the BIT
	if (boost::binary_search(andbits, andbit)) {
		LAZY_BC_LOG_DEBUG << "The following and-BIT is already in the BIT:"
		                  << std::endl << andbit.to_string();
		return nullptr;
	} else {
		// Check that it is not alpha-equivalent either
		for (const AndBIT& ab : andbits) {
			if (ScopeLinkCast(andbit.fcs)->is_equal(ab.fcs)) {
				LAZY_BC_LOG_DEBUG << "The following and-BIT is alpha-equivalent "
				                  << "to another one already in the BIT:"
				                  << std::endl << andbit.to_string();
				return nullptr;
			}
		}
	}

	// Insert while keeping the order
	auto it = andbits.insert(boost::lower_bound(andbits, andbit), andbit);

	// Return andbit pointer
	return &*it;
}

BIT::AndBITs::iterator BIT::erase(BIT::AndBITs::iterator from,
                                  BIT::AndBITs::iterator to)
{
	return andbits.erase(from, to);
}

void BIT::reset_exhausted_flags()
{
	for (AndBIT& andbit : andbits)
		andbit.reset_exhausted();
}

bool BIT::is_in(const Rule& rule, const BITNode& bitnode) const
{
	for (const Rule& bnr : bitnode.rules)
		if (rule.is_alpha_equivalent(bnr))
			return true;
	return false;
}

std::string oc_to_string(const BITNode& bitnode)
{
	return bitnode.to_string();
}

std::string oc_to_string(const AndBIT& andbit)
{
	return andbit.to_string();
}

// std::string oc_to_string(const BIT& bit)
// {
// 	return bit.to_string();
// }

} // ~namespace opencog
