/**
 * Unify.cc
 *
 * Utilities for unifying atoms.
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
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

#include "Unify.h"

#include <opencog/util/algorithm.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomutils/FindUtils.h>

namespace opencog {

Unify::SolutionSet::SolutionSet(bool s, const Unify::Partitions& p)
	: satisfiable(s), partitions(p) {}

bool Unify::SolutionSet::operator==(const SolutionSet& other) const
{
	return satisfiable == other.satisfiable
		and partitions == other.partitions;
}

Unify::TypedSubstitutions Unify::typed_substitutions(const SolutionSet& sol,
                                                     const Handle& pre,
                                                     const Handle& lhs,
                                                     const Handle& rhs,
                                                     Handle lhs_vardecl,
                                                     Handle rhs_vardecl) const
{
	OC_ASSERT(sol.satisfiable);

	TypedSubstitutions result;
	for (const Partition& partition : sol.partitions) {
		std::pair<HandleMap, Handle> ts;
		for (const Partition::value_type& typed_block : partition) {
			Handle least_abstract(Handle(createNode(VARIABLE_NODE,
			                                        "__dummy_top__")));
			for (const Handle& h : typed_block.first) {
				// Find the least abstract atom
				if (inherit(h, least_abstract) and
				    // If h is a variable, only consider it as value
				    // if it is in pre (stands for precedence)
				    (h->getType() != VARIABLE_NODE
				     or is_unquoted_unscoped_in_tree(pre, h)))
					least_abstract = h;
			}
			// Build variable mapping
			for (const Handle& var : typed_block.first) {
				if (var->getType() == VARIABLE_NODE)
					ts.first.insert({var, least_abstract});
			}
		}
		// Build the type for this variable. For now, the type is
		// merely lhs_vardecl and rhs_vardecl merged together. To do
		// well it should be taking into account the possibly more
		// restrictive types found during unification (i.e. the block
		// types).
		//
		// TODO: variables without declaration (i.e. rhs_vardecl or
		// lhs_vardecl are undefined) should be added here, borrowing
		// the variable declarations of equivalent variables if any.
		if (lhs.is_defined() and lhs_vardecl.is_undefined())
			lhs_vardecl = gen_vardecl(lhs);
		if (rhs.is_defined() and rhs_vardecl.is_undefined())
			rhs_vardecl = gen_vardecl(rhs);
		ts.second = merge_vardecl(rhs_vardecl, lhs_vardecl);

		result.insert(ts);
	}
	return result;
}

bool Unify::is_ill_quotation(BindLinkPtr bl) const
{
	return bl->get_vardecl().is_undefined();
}

bool Unify::is_pm_connector(const Handle& h) const
{
	return is_pm_connector(h->getType());
}

bool Unify::is_pm_connector(Type t) const
{
	return t == AND_LINK or t == OR_LINK or t == NOT_LINK;
}

BindLinkPtr Unify::consume_ill_quotations(BindLinkPtr bl) const
{
	Handle vardecl = bl->get_vardecl(),
		pattern = bl->get_body(),
		rewrite = bl->get_implicand();

	// Consume the pattern's quotations
	pattern = consume_ill_quotations(bl->get_variables(), pattern);

	// Consume the rewrite's quotations
	rewrite = consume_ill_quotations(bl->get_variables(), rewrite);

	// If the pattern has clauses with free variables but no vardecl
	// it means that some quotations are missing. Rather than adding
	// them we merely set vardecl to an empty VariableList.
	if (not vardecl and not get_free_variables(pattern).empty())
		vardecl = Handle(createVariableList(HandleSeq{}));

	// Recreate the BindLink
	return vardecl ?
		createBindLink(vardecl, pattern, rewrite)
		: createBindLink(pattern, rewrite);
}

Handle Unify::consume_ill_quotations(const Variables& variables, Handle h,
                                     Quotation quotation, bool escape) const
{
	// Base case
	if (h->isNode())
		return h;

	// Recursive cases
	Type t = h->getType();
	if (quotation.consumable(t)) {
		if (t == QUOTE_LINK) {
			Handle scope = h->getOutgoingAtom(0);
			OC_ASSERT(classserver().isA(scope->getType(), SCOPE_LINK),
			          "This defaults the assumption, see this function comment");
			// Check whether the vardecl of scope is bound to the
			// ancestor scope rather than itself, if so escape the
			// consumption.
			if (not is_bound_to_ancestor(variables, scope)) {
				quotation.update(t);
				return consume_ill_quotations(variables, scope, quotation);
			} else {
				escape = true;
			}
		} else if (t == UNQUOTE_LINK) {
			if (not escape) {
				quotation.update(t);
				return consume_ill_quotations(variables, h->getOutgoingAtom(0),
				                              quotation);
			}
		}
		// Ignore LocalQuotes as they supposedly used only to quote
		// pattern matcher connectors.
	}

	quotation.update(t);
	HandleSeq consumed;
	for (const Handle outh : h->getOutgoingSet())
		consumed.push_back(consume_ill_quotations(variables, outh, quotation,
		                                          escape));

	// TODO: call all factories
	bool is_scope = classserver().isA(t, SCOPE_LINK);
	return is_scope ? Handle(ScopeLink::factory(t, consumed))
		: Handle(createLink(t, consumed));
}

bool Unify::is_bound_to_ancestor(const Variables& variables,
                                 const Handle& local_scope) const
{
	Handle unquote = local_scope->getOutgoingAtom(0);
	if (unquote->getType() == UNQUOTE_LINK) {
		Handle var = unquote->getOutgoingAtom(0);
		return variables.is_in_varset(var);
	}
	return false;
}

Handle Unify::substitute(BindLinkPtr bl, const TypedSubstitution& ts) const
{
	// Get the list of values to substitute from ts
	HandleSeq values = bl->get_variables().make_values(ts.first);

	// Perform alpha-conversion, this will work over values that are
	// non variables as well
	//
	// TODO: make sure that ts.second contains the declaration of all
	// variables
	Handle h = bl->alpha_conversion(values, ts.second);

	return Handle(consume_ill_quotations(BindLinkCast(h)));
}

Unify::SolutionSet Unify::operator()(const Handle& lhs, const Handle& rhs,
                                     const Handle& lhs_vardecl,
                                     const Handle& rhs_vardecl,
                                     Quotation lhs_quotation,
                                     Quotation rhs_quotation)
{
	_lhs_vardecl = lhs_vardecl;
	_rhs_vardecl = rhs_vardecl;
	return unify(lhs, rhs, lhs_quotation, rhs_quotation);
}

Unify::SolutionSet Unify::unify(const Handle& lhs, const Handle& rhs,
                                Quotation lhs_quotation,
                                Quotation rhs_quotation) const
{
	///////////////////
	// Base cases    //
	///////////////////

	// Make sure both handles are defined
	if (not lhs or not rhs)
		return SolutionSet(false);

	Type lhs_type(lhs->getType());
	Type rhs_type(rhs->getType());

	// If one is a node
	if (lhs->isNode() or rhs->isNode()) {
		// If one is an unquoted variable, then unifies, otherwise
		// check their equality
		if ((lhs_quotation.is_unquoted() and lhs_type == VARIABLE_NODE)
		    or (rhs_quotation.is_unquoted() and rhs_type == VARIABLE_NODE)) {
			return mkvarsol(lhs, rhs, lhs_quotation, rhs_quotation);
		} else
			return SolutionSet(lhs == rhs);
	}

	////////////////////////
	// Recursive cases    //
	////////////////////////

	// Consume quotations
	if (lhs_quotation.consumable(lhs_type)
	    and rhs_quotation.consumable(rhs_type)) {
		lhs_quotation.update(lhs_type);
		rhs_quotation.update(rhs_type);
		return unify(lhs->getOutgoingAtom(0), rhs->getOutgoingAtom(0),
		             lhs_quotation, rhs_quotation);
	}
	if (lhs_quotation.consumable(lhs_type)) {
		lhs_quotation.update(lhs_type);
		return unify(lhs->getOutgoingAtom(0), rhs, lhs_quotation, rhs_quotation);
	}
	if (rhs_quotation.consumable(rhs_type)) {
		rhs_quotation.update(rhs_type);
		return unify(lhs, rhs->getOutgoingAtom(0), lhs_quotation, rhs_quotation);
	}

	// Update quotations
	lhs_quotation.update(lhs_type);
	rhs_quotation.update(rhs_type);

	// At least one of them is a link, check if they have the same
	// type (e.i. do they match so far)
	if (lhs_type != rhs_type)
		return SolutionSet(false);

	// At this point they are both links of the same type, check that
	// they have the same arity
	Arity lhs_arity(lhs->getArity());
	Arity rhs_arity(rhs->getArity());
	if (lhs_arity != rhs_arity)
		return SolutionSet(false);

	if (is_unordered(rhs))
		return unordered_unify(lhs->getOutgoingSet(), rhs->getOutgoingSet(),
		                       lhs_quotation, rhs_quotation);
	else
		return ordered_unify(lhs->getOutgoingSet(), rhs->getOutgoingSet(),
		                     lhs_quotation, rhs_quotation);
}

Unify::SolutionSet Unify::unordered_unify(const HandleSeq& lhs,
                                          const HandleSeq& rhs,
                                          Quotation lhs_quotation,
                                          Quotation rhs_quotation) const
{
	Arity lhs_arity(lhs.size());
	Arity rhs_arity(rhs.size());
	OC_ASSERT(lhs_arity == rhs_arity);

	// Base case
	if (lhs_arity == 0)
		return SolutionSet();

	// Recursive case
	SolutionSet sol(false);
	for (Arity i = 0; i < lhs_arity; ++i) {
		auto head_sol = unify(lhs[i], rhs[0], lhs_quotation, rhs_quotation);
		if (head_sol.satisfiable) {
			HandleSeq lhs_tail(cp_erase(lhs, i));
			HandleSeq rhs_tail(cp_erase(rhs, 0));
			auto tail_sol = unordered_unify(lhs_tail, rhs_tail,
			                                lhs_quotation, rhs_quotation);
			SolutionSet perm_sol = join(head_sol, tail_sol);
			// Union merge satisfiable permutations
			if (perm_sol.satisfiable) {
				sol.satisfiable = true;
				sol.partitions.insert(perm_sol.partitions.begin(),
				                      perm_sol.partitions.end());
			}
		}
	}
	return sol;
}

Unify::SolutionSet Unify::ordered_unify(const HandleSeq& lhs,
                                        const HandleSeq& rhs,
                                        Quotation lhs_quotation,
                                        Quotation rhs_quotation) const
{
	Arity lhs_arity(lhs.size());
	Arity rhs_arity(rhs.size());
	OC_ASSERT(lhs_arity == rhs_arity);

	SolutionSet sol;
	for (Arity i = 0; i < lhs_arity; ++i) {
		auto rs = unify(lhs[i], rhs[i], lhs_quotation, rhs_quotation);
		sol = join(sol, rs);
		if (not sol.satisfiable)     // Stop if unification has failed
			break;
	}
	return sol;
}

Unify::SolutionSet Unify::comb_unify(const OrderedHandleSet& lhs,
                                     const OrderedHandleSet& rhs,
                                     Quotation lhs_quotation,
                                     Quotation rhs_quotation) const
{
	SolutionSet sol;
	for (const Handle& lh : lhs) {
		for (const Handle& rh : rhs) {
			auto rs = unify(lh, rh, lhs_quotation, rhs_quotation);
			sol = join(sol, rs);
			if (not sol.satisfiable)     // Stop if unification has failed
				return sol;
		}
	}
	return sol;
}
	
bool Unify::is_unordered(const Handle& h) const
{
	return classserver().isA(h->getType(), UNORDERED_LINK);
}

HandleSeq Unify::cp_erase(const HandleSeq& hs, Arity i) const
{
	HandleSeq hs_cp(hs);
	hs_cp.erase(hs_cp.begin() + i);
	return hs_cp;
}

Unify::SolutionSet Unify::mkvarsol(const Handle& lhs, const Handle& rhs,
                                   Quotation lhs_quotation,
                                   Quotation rhs_quotation) const
{
	Handle inter = type_intersection(lhs, rhs, _lhs_vardecl, _rhs_vardecl,
	                                 lhs_quotation, rhs_quotation);
	if (not inter)
		return SolutionSet(false);
	else {
		OrderedHandleSet hset{lhs, rhs};
		Partitions par{{{hset, inter}}};
		return SolutionSet(true, par);
	}
}

Unify::SolutionSet Unify::join(const SolutionSet& lhs,
                               const SolutionSet& rhs) const
{
	// No need to join if one of them is non satisfiable
	if (not lhs.satisfiable or not rhs.satisfiable)
		return SolutionSet(false);

	// No need to join if one of them is empty
	if (rhs.partitions.empty())
		return lhs;
	if (lhs.partitions.empty())
		return rhs;

	// By now both are satisfiable and non empty, join them
	SolutionSet result;
	for (const Partition& rp : rhs.partitions) {
		Partitions sol(join(lhs.partitions, rp));
		result.partitions.insert(sol.begin(), sol.end());
	}

	// If we get an empty join while the inputs where not empty then
	// the join has failed
	result.satisfiable = not result.partitions.empty();

	return result;
}

Unify::Partitions Unify::join(const Partitions& lhs, const Partition& rhs) const
{
	// Base cases
	if (rhs.empty())
		return lhs;
	if (lhs.empty())
		return {rhs};

	// Recursive case (a loop actually)
	Partitions result;
	for (const auto& par : lhs) {
		Partitions jps = join(par, rhs);
		result.insert(jps.begin(), jps.end());
	}
	return result;
}

Unify::Partitions Unify::join(const Partition& lhs, const Partition& rhs) const
{
	// Don't bother joining if lhs is empty (saves a bit of computation)
	if (lhs.empty())
		return {rhs};

	// Join
	Partitions result{lhs};
	for (const Block& rhs_block : rhs) {
		// For now we assume result has only 0 or 1 partition
		result = join(result, rhs_block);
		if (result.empty())
			break;              // If empty, break cause not satisfiable
	}

	return result;
}

Unify::Partitions Unify::join(const Partitions& partitions,
                              const Block& block) const
{
	Partitions result;
	for (const Partition& partition : partitions) {
		Partitions jps = join(partition, block);
		result.insert(jps.begin(), jps.end());
	}
	return result;
}

Unify::Partitions Unify::join(const Partition& partition,
                              const Block& block) const
{
	// Find all partition blocks that have elements in common with block
	std::vector<Block> common_blocks;
	for (const Block& p_block : partition)
		if (not has_empty_intersection(block.first, p_block.first))
			common_blocks.push_back(p_block);

	Partition jp(partition);
	if (common_blocks.empty()) {
		// If none then merely insert the independent block
		jp.insert(block);
		return {jp};
	} else {
		// Otherwise join block with all common blocks and replace
		// them by the result (if satisfiable, otherwise return the
		// empty partition)
		Block j_block = join(common_blocks, block);
		if (is_satisfiable(j_block)) {
			for (const Block& rm : common_blocks)
				jp.erase(rm.first);
			jp.insert(j_block);

			// Perform the sub-unification of all common blocks with
			// block and join the solution set to jp
			SolutionSet sol = subunify(common_blocks, block);
			if (sol.satisfiable)
				return join(sol.partitions, jp);
		}
		return Partitions();
	}
}

Unify::Block Unify::join(const std::vector<Block>& common_blocks,
                         const Block& block) const
{
	std::pair<OrderedHandleSet, Handle> result{block};
	for (const auto& c_block : common_blocks)
		result =  join(result, c_block);
	return result;
}

Unify::Block Unify::join(const Block& lhs, const Block& rhs) const
{
	return {set_union(lhs.first, rhs.first),
			type_intersection(lhs.second, rhs.second)};
}

Unify::SolutionSet Unify::subunify(const std::vector<Block>& common_blocks,
                                   const Block& block) const
{
	SolutionSet sol;
	for (const Block& c_block : common_blocks) {
		SolutionSet rs = subunify(c_block, block);
		sol = join(sol, rs);
		if (not sol.satisfiable)     // Stop if unification has failed
			break;
	}
	return sol;
}

Unify::SolutionSet Unify::subunify(const Block& lhs, const Block& rhs) const
{
	return comb_unify(set_difference(lhs.first, rhs.first),
	                  set_difference(rhs.first, lhs.first));
}

bool Unify::is_satisfiable(const Block& block) const
{
	return (bool)block.second;
}

// TODO: very limited type intersection, should support structural
// types, etc.
Handle type_intersection(const Handle& lhs, const Handle& rhs,
                         const Handle& lhs_vardecl, const Handle& rhs_vardecl,
                         Quotation lhs_quotation, Quotation rhs_quotation)
{
	if (inherit(lhs, rhs, lhs_vardecl, rhs_vardecl,
	            lhs_quotation, rhs_quotation))
		return lhs;
	if (inherit(rhs, lhs, rhs_vardecl, lhs_vardecl,
	            rhs_quotation, lhs_quotation))
		return rhs;
	return Handle::UNDEFINED;
}

std::set<Type> simplify_type_union(std::set<Type>& type)
{
	return {}; // TODO: do we really need that?
}

std::set<Type> get_union_type(const Handle& h, const Handle& vardecl)
{
	VariableListPtr vardecl_vlp(gen_varlist(h, vardecl));
	const Variables& variables(vardecl_vlp->get_variables());
	const VariableTypeMap& vtm = variables._simple_typemap;
	auto it = vtm.find(h);
	if (it == vtm.end() or it->second.empty())
		return {ATOM};
	else {
		return it->second;
	}
}

bool inherit(const Handle& lhs, const Handle& rhs,
             const Handle& lhs_vardecl, const Handle& rhs_vardecl,
             Quotation lhs_quotation, Quotation rhs_quotation)
{
	Type lhs_type = lhs->getType();
	Type rhs_type = rhs->getType();

	// Recursive cases

	// Consume quotations
	if (lhs_quotation.consumable(lhs_type)) {
		lhs_quotation.update(lhs_type);
		return inherit(lhs->getOutgoingAtom(0), rhs, lhs_vardecl, rhs_vardecl,
		               lhs_quotation, rhs_quotation);
	}
	if (rhs_quotation.consumable(rhs_type)) {
		rhs_quotation.update(rhs_type);
		return inherit(lhs, rhs->getOutgoingAtom(0), lhs_vardecl, rhs_vardecl,
		               lhs_quotation, rhs_quotation);
	}

	// If both are links then check that the outgoings of lhs inherit
	// the outgoings of rhs.
	if (lhs->isLink() and rhs->isLink() and (lhs_type == rhs_type)) {
		if (lhs->getArity() == rhs->getArity()) {
			for (size_t i = 0; i < lhs->getArity(); i++) {
				if (not inherit(lhs->getOutgoingAtom(i),
				                rhs->getOutgoingAtom(i),
				                lhs_vardecl, rhs_vardecl,
				                lhs_quotation, rhs_quotation))
					return false;
			}
			return true;
		} else return false;
	}

	// Base cases

	if (lhs == rhs)
		return true;

	if (lhs_quotation.is_unquoted() and VARIABLE_NODE == lhs_type
	    and rhs_quotation.is_unquoted() and VARIABLE_NODE == rhs_type)
		return inherit(get_union_type(lhs, lhs_vardecl),
		               get_union_type(rhs, rhs_vardecl));

	if (rhs_quotation.is_unquoted() and VARIABLE_NODE == rhs_type)
		return gen_varlist(rhs, rhs_vardecl)->is_type(rhs, lhs);

	return false;
}

bool inherit(const Handle& lhs, const Handle& rhs)
{
	return VARIABLE_NODE == rhs->getType() or lhs == rhs;
}

bool inherit(Type lhs, Type rhs)
{
	return classserver().isA(lhs, rhs);
}

bool inherit(Type lhs, const std::set<Type>& rhs)
{
	for (Type ty : rhs)
		if (inherit(lhs, ty))
			return true;
	return false;
}

bool inherit(const std::set<Type>& lhs, const std::set<Type>& rhs)
{
	for (Type ty : lhs)
		if (not inherit(ty, rhs))
			return false;
	return true;
}

/**
 * Generate a VariableList of the free variables of a given atom h.
 */
VariableListPtr gen_varlist(const Handle& h)
{
	OrderedHandleSet vars = get_free_variables(h);
	return createVariableList(HandleSeq(vars.begin(), vars.end()));
}

Handle gen_vardecl(const Handle& h)
{
	return Handle(gen_varlist(h));
}

/**
 * Given an atom h and its variable declaration vardecl, turn the
 * vardecl into a VariableList if not already, and if undefined,
 * generate a VariableList of the free variables of h.
 */
VariableListPtr gen_varlist(const Handle& h, const Handle& vardecl)
{
	if (not vardecl)
		return gen_varlist(h);
	else {
		Type vardecl_t = vardecl->getType();
		if (vardecl_t == VARIABLE_LIST)
			return VariableListCast(vardecl);
		else {
			OC_ASSERT(vardecl_t == VARIABLE_NODE
			          or vardecl_t == TYPED_VARIABLE_LINK);
			return createVariableList(vardecl);
		}
	}
}

Handle merge_vardecl(const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	if (not lhs_vardecl)
		return rhs_vardecl;
	if (not rhs_vardecl)
		return lhs_vardecl;

	VariableList
		lhs_vl(lhs_vardecl),
		rhs_vl(rhs_vardecl);

	const Variables& lhs_vars = lhs_vl.get_variables();
	Variables new_vars = rhs_vl.get_variables();

	new_vars.extend(lhs_vars);

	return new_vars.get_vardecl();
}

std::string oc_to_string(const Unify::Block& ub)
{
	std::stringstream ss;
	ss << "block:" << std::endl << oc_to_string(ub.first)
	   << "type:" << std::endl << oc_to_string(ub.second);
	return ss.str();
}

std::string oc_to_string(const Unify::Partition& up)
{
	std::stringstream ss;
	ss << "size = " << up.size() << std::endl;
	int i = 0;
	for (const auto& p : up) {
		ss << "block[" << i << "]:" << std::endl << oc_to_string(p.first)
		   << "type[" << i << "]:" << std::endl << oc_to_string(p.second);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const Unify::Partitions& par)
{
	std::stringstream ss;
	ss << "size = " << par.size() << std::endl;
	int i = 0;
	for (const auto& el : par) {
		ss << "typed partition[" << i << "]:" << std::endl << oc_to_string(el);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const Unify::SolutionSet& sol)
{
	std::stringstream ss;
	ss << "satisfiable: " << sol.satisfiable << std::endl
	   << "partitions: " << oc_to_string(sol.partitions);
	return ss.str();
}

std::string oc_to_string(const Unify::TypedSubstitutions& tss)
{
	std::stringstream ss;
	ss << "size = " << tss.size() << std::endl;
	int i = 0;
	for (const auto& ts : tss) {
		ss << "typed substitution[" << i << "]:" << std::endl
		   << oc_to_string(ts);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const Unify::TypedSubstitution& ts)
{
	std::stringstream ss;
	ss << "substitution:" << std::endl << oc_to_string(ts.first)
	   << "vardecl:" << std::endl << oc_to_string(ts.second);
	return ss.str();
}

} // namespace opencog
