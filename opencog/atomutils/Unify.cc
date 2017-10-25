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
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atoms/pattern/PatternUtils.h>

namespace opencog {

const Unify::Partitions Unify::empty_partitions({});

const Unify::Partitions Unify::empty_partition_singleton({{}});

Unify::CHandle::CHandle(const Handle& h, const Context& c)
	: handle(h), context(c) {}

bool Unify::CHandle::is_variable() const
{
	return handle->get_type() == VARIABLE_NODE;
}

bool Unify::CHandle::is_free_variable() const
{
	return context.is_free_variable(handle);
}

HandleSet Unify::CHandle::get_free_variables() const
{
	HandleSet free_vars =
		opencog::get_free_variables(handle, context.quotation);
	return set_difference(free_vars, context.shadow);
}

Context::VariablesStack::const_iterator
Unify::CHandle::find_variables(const Handle& h) const
{
	return std::find_if(context.scope_variables.cbegin(),
	                    context.scope_variables.cend(),
	                    [&](const Variables& variables) {
		                    return variables.is_in_varset(h);
	                    });
}

bool Unify::CHandle::is_consumable() const
{
	return context.quotation.consumable(handle->get_type());
}

bool Unify::CHandle::is_quoted() const
{
	return context.quotation.is_quoted();
}

bool Unify::CHandle::is_unquoted() const
{
	return context.quotation.is_unquoted();
}

void Unify::CHandle::update()
{
	bool isc = is_consumable();
	context.update(handle);
	if (isc)
		handle = handle->getOutgoingAtom(0);
}

bool Unify::CHandle::is_node_satisfiable(const CHandle& other) const
{
	// If both are variable check whether they could be alpha
	// equivalent, otherwise merely check for equality
	if (is_variable() and other.is_variable())	{
		// Make sure scope variable declarations are stored
		OC_ASSERT(context.store_scope_variables,
		          "You must store the scope variable declarations "
		          "in order to use this method");

		// Search variable declarations associated to the variables
		Context::VariablesStack::const_iterator it = find_variables(handle),
			other_it = other.find_variables(other.handle);
		OC_ASSERT(it != context.scope_variables.cend(),
		          "Contradicts the assumption that this->handle is not free");
		OC_ASSERT(other_it != other.context.scope_variables.cend(),
		          "Contradicts the assumption that other.handle is not free");

		// Check that both variable declarations occured at the same level
		if (std::distance(context.scope_variables.cbegin(), it)
		    != std::distance(other.context.scope_variables.cbegin(), other_it))
			return false;

		// Check that the other variable is alpha convertible
		return it->is_alpha_convertible(handle, other.handle, *other_it, true);
	} else {
		return content_eq(handle, other.handle);
	}
}

bool Unify::CHandle::operator==(const CHandle& ch) const
{
	return content_eq(handle, ch.handle) and (context == ch.context);
}

bool Unify::CHandle::operator<(const CHandle& ch) const
{
	return (handle < ch.handle) or
		(handle == ch.handle and context < ch.context);
}

Unify::SolutionSet::SolutionSet(bool s)
	: Partitions(s ? empty_partition_singleton : empty_partitions) {}

Unify::SolutionSet::SolutionSet(const Unify::Partitions& p)
	: Partitions(p) {}

bool Unify::SolutionSet::is_satisfiable() const
{
	return not empty();
}

Unify::Unify(const Handle& lhs, const Handle& rhs,
             const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	// Set terms to unify
	_lhs = lhs;
	_rhs = rhs;

	// Set _variables
	set_variables(lhs, rhs, lhs_vardecl, rhs_vardecl);
}

Unify::TypedSubstitutions Unify::typed_substitutions(const SolutionSet& sol,
                                                     const Handle& pre) const
{
	OC_ASSERT(sol.is_satisfiable());

	TypedSubstitutions result;
	for (const Partition& partition : sol)
		result.insert(typed_substitution(partition, pre));
	return result;
}

Unify::TypedSubstitution Unify::typed_substitution(const Partition& partition,
                                                   const Handle& pre) const
{
	// Associate the least abstract element to each variable of each
	// block.
	HandleCHandleMap var2cval;
	for (const TypedBlock& block : partition) {
		CHandle least_abstract = find_least_abstract(block, pre);

		// Build variable mapping
		for (const CHandle& ch : block.first)
			if (ch.is_free_variable())
				var2cval.insert({ch.handle, least_abstract});
	}

	// Calculate its closure
	var2cval = substitution_closure(var2cval);

	// Remove ill quotations
	for (auto& vcv : var2cval) {
		Handle consumed = consume_ill_quotations(_variables, vcv.second.handle,
		                                         vcv.second.context.quotation);
		vcv.second = CHandle(consumed, vcv.second.context);
	}

	// Calculate its variable declaration
	Handle vardecl = substitution_vardecl(var2cval);

	// Return the typed substitution
	return {var2cval, vardecl};
}

void Unify::set_variables(const Handle& lhs, const Handle& rhs,
                          const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	// Merge the 2 type declarations
	Variables lv = gen_varlist(lhs, lhs_vardecl)->get_variables();
	Variables rv = gen_varlist(rhs, rhs_vardecl)->get_variables();
	_variables = merge_variables(lv, rv);
}

Unify::CHandle Unify::find_least_abstract(const TypedBlock& block,
                                          const Handle& pre) const
{
	static Handle top(Handle(createNode(VARIABLE_NODE, "__dummy_top__")));
	CHandle least_abstract(top);
	for (const CHandle& ch : block.first) {
		if (inherit(ch, least_abstract) and
		    // If h is a variable, only consider it as value
		    // if it is in pre (stands for precedence)
		    (not ch.is_variable()
		     or is_unquoted_unscoped_in_tree(pre, ch.handle))) {
			least_abstract = ch;
		}
	}

	OC_ASSERT(least_abstract.handle != top,
	          "Finding the least abstract atom in the block has failed. "
	          "It is probably a bug.");

	return least_abstract;
}

Unify::HandleCHandleMap Unify::substitution_closure(const HandleCHandleMap& var2cval) const
{
	// Strip var2cval from its contexts
	HandleMap var2val = strip_context(var2cval);

	// Subtitute every value that have variables by other values
	// associated to these variables.
	HandleCHandleMap result(var2cval);
	for (auto& el : result) {
		VariableListPtr varlist = gen_varlist(el.second);
		const Variables& variables = varlist->get_variables();
		HandleSeq values = variables.make_values(var2val);
		el.second.handle = variables.substitute_nocheck(el.second.handle, values);
	}

	// If we have reached a fixed point then return substitution,
	// otherwise re-iterate
	return hchm_content_eq(result, var2cval) ?
		result : substitution_closure(result);
}

Handle Unify::substitution_vardecl(const HandleCHandleMap& var2val) const
{
	// Build the type declaration for this substitution. For now, the
	// type is merely lhs_vardecl and rhs_vardecl merged together,
	// then all variables assigned for substitution other than
	// themselves are removed. To do well it should be taking into
	// account the possibly more restrictive types found during
	// unification (i.e. the block types).

	Variables ts_variables = _variables;

	for (const auto& el : var2val)
		// Make sure it is not a self substitution
		if (el.first != el.second.handle)
			ts_variables.erase(el.first);

	return ts_variables.get_vardecl();
}

bool Unify::is_pm_connector(const Handle& h)
{
	return is_pm_connector(h->get_type());
}

bool Unify::is_pm_connector(Type t)
{
	return t == AND_LINK or t == OR_LINK or t == NOT_LINK;
}

BindLinkPtr Unify::consume_ill_quotations(BindLinkPtr bl)
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

Handle Unify::consume_ill_quotations(const Handle& vardecl, const Handle& h)
{
	const Variables variables = createVariableList(vardecl)->get_variables();
	return consume_ill_quotations(variables, h);
}

Handle Unify::consume_ill_quotations(const Variables& variables, Handle h,
                                     Quotation quotation, bool escape)
{
	// Base case
	if (h->is_node())
		return h;

	// Recursive cases
	Type t = h->get_type();
	if (quotation.consumable(t)) {
		if (t == QUOTE_LINK) {
			Handle scope = h->getOutgoingAtom(0);
			OC_ASSERT(classserver().isA(scope->get_type(), SCOPE_LINK),
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

	return createLink(consumed, t);
}

bool Unify::is_bound_to_ancestor(const Variables& variables,
                                 const Handle& local_scope)
{
	Handle unquote = local_scope->getOutgoingAtom(0);
	if (unquote->get_type() == UNQUOTE_LINK) {
		Handle var = unquote->getOutgoingAtom(0);
		return variables.is_in_varset(var);
	}
	return false;
}

Handle Unify::substitute(BindLinkPtr bl, const TypedSubstitution& ts)
{
	// TODO: make sure that ts.second contains the declaration of all
	// variables
	return substitute(bl, strip_context(ts.first), ts.second);
}

Handle Unify::substitute(BindLinkPtr bl, const HandleMap& var2val,
                         Handle vardecl)
{
	// Perform substitution over the existing variable declaration, if
	// no new alternative is provided.
	if (not vardecl) {
		// If the bind link has no variable declaration either then
		// infer one
		Handle old_vardecl = bl->get_vardecl() ? bl->get_vardecl()
			: gen_vardecl(bl->get_body());
		// Substitute the variables in the old vardecl to obtain the
		// new one.
		vardecl = substitute_vardecl(bl->get_vardecl(), var2val);
	}

	const Variables variables = bl->get_variables();

	// Turn the map into a vector of new variable names/values
	HandleSeq values = variables.make_values(var2val);

	// Substituted BindLink outgoings
	HandleSeq hs;

	// Perform substitution over the pattern term, then remove
	// constant clauses
	Handle clauses = variables.substitute_nocheck(bl->get_body(), values);
	clauses = consume_ill_quotations(vardecl, clauses);
	clauses = remove_constant_clauses(vardecl, clauses);
	hs.push_back(clauses);

	// Perform substitution over the rewrite term
	Handle rewrite = variables.substitute_nocheck(bl->get_implicand(), values);
	rewrite = consume_ill_quotations(vardecl, rewrite);
	hs.push_back(rewrite);

	// Filter vardecl
	vardecl = filter_vardecl(vardecl, hs);

	// Insert vardecl in hs if defined
	if (vardecl)
		hs.insert(hs.begin(), vardecl);

	// Create the substituted BindLink
	return createLink(hs, bl->get_type());
}

Handle Unify::substitute_vardecl(const Handle& vardecl,
                                 const HandleMap& var2val)
{
	if (not vardecl)
		return Handle::UNDEFINED;

	Type t = vardecl->get_type();

	// Base cases

	if (t == VARIABLE_NODE) {
		auto it = var2val.find(vardecl);
		// Only substitute if the variable is substituted by another variable
		if (it != var2val.end() and it->second->get_type() == VARIABLE_NODE)
			return it->second;
		return Handle::UNDEFINED;
	}

	// Recursive cases

	HandleSeq oset;

	if (t == VARIABLE_LIST) {
		for (const Handle& h : vardecl->getOutgoingSet()) {
			Handle nh = substitute_vardecl(h, var2val);
			if (nh)
				oset.push_back(nh);
		}
		if (oset.empty())
			return Handle::UNDEFINED;
	}
	else if (t == TYPED_VARIABLE_LINK) {
		Handle new_var = substitute_vardecl(vardecl->getOutgoingAtom(0),
		                                    var2val);
		if (new_var) {
			oset.push_back(new_var);
			oset.push_back(vardecl->getOutgoingAtom(1));
		} else return Handle::UNDEFINED;
	}
	else {
		OC_ASSERT(false, "Not implemented");
	}
	return createLink(oset, t);
}

// TODO: for now it is assumed clauses are connected by an AndLink
// only. To fix that one needs to generalize
// PatternLink::unbundle_clauses to make it usable in that code too.
//
// TODO: maybe replace Handle vardecl by Variables variables.
Handle Unify::remove_constant_clauses(const Handle& vardecl,
                                      const Handle& clauses)
{
	VariableListPtr vl = createVariableList(vardecl);
	HandleSet vars = vl->get_variables().varset;

	// Remove constant clauses
	Type t = clauses->get_type();
	HandleSeq hs;
	if (t == AND_LINK) {
		for (const Handle& clause : clauses->getOutgoingSet()) {
			if (not is_constant(vars, clause)) {
				hs.push_back(clause);
			}
		}
	} else if (not is_constant(vars, clauses)) {
		return clauses;
	}
	return createLink(hs, AND_LINK);
}

Unify::SolutionSet Unify::operator()()
{
	// If the declaration is ill typed, there is no solution
	if (not _variables.is_well_typed())
		return SolutionSet();

	// It is well typed, perform the unification
	return unify(_lhs, _rhs);
}

Unify::SolutionSet Unify::unify(const CHandle& lhs, const CHandle& rhs) const
{
	return unify(lhs.handle, rhs.handle, lhs.context, rhs.context);
}

Unify::SolutionSet Unify::unify(const Handle& lh, const Handle& rh,
                                Context lc, Context rc) const
{
	Type lt(lh->get_type());
	Type rt(rh->get_type());

	///////////////////
	// Base cases    //
	///////////////////

	// Make sure both handles are defined
	if (not lh or not rh)
		return SolutionSet();

	CHandle lch(lh, lc);
	CHandle rch(rh, rc);

	// If one is a node
	if (lh->is_node() or rh->is_node()) {
		// If one is a free variable and they are different, then
		// unifies.
		if (lch.is_free_variable() or rch.is_free_variable()) {
			if (lch == rch) {
				// Do not construct a solution like {X}->X to not
				// overload the solution set.
				//
				// Since the context is taken into account they have
				// the same context, thus if one of them is free, the
				// other is free as well, therefore they are
				// satisfiable.
				return SolutionSet(true);
			} else {
				return mkvarsol(lch, rch);
			}
		} else
			return SolutionSet(lch.is_node_satisfiable(rch));
	}

	////////////////////////
	// Recursive cases    //
	////////////////////////

    // Consume quotations
	bool lq = lc.quotation.consumable(lt);
	bool rq = rc.quotation.consumable(rt);
	if (lq and rq) {
		lc.quotation.update(lt);
		rc.quotation.update(rt);
		return unify(lh->getOutgoingAtom(0), rh->getOutgoingAtom(0), lc, rc);
	}
	if (lq) {
		lc.quotation.update(lt);
		return unify(lh->getOutgoingAtom(0), rh, lc, rc);
	}
	if (rq) {
		rc.quotation.update(rt);
		return unify(lh, rh->getOutgoingAtom(0), lc, rc);
	}

	// Update contexts
	lc.update(lh);
	rc.update(rh);

	// At least one of them is a link, check if they have the same
	// type (e.i. do they match so far)
	if (lt != rt)
		return SolutionSet();

	// At this point they are both links of the same type, check that
	// they have the same arity
	Arity lh_arity(lh->get_arity());
	Arity rh_arity(rh->get_arity());
	if (lh_arity != rh_arity)
		return SolutionSet();

	if (is_unordered(rh))
		return unordered_unify(lh->getOutgoingSet(), rh->getOutgoingSet(), lc, rc);
	else
		return ordered_unify(lh->getOutgoingSet(), rh->getOutgoingSet(), lc, rc);
}

Unify::SolutionSet Unify::unordered_unify(const HandleSeq& lhs,
                                          const HandleSeq& rhs,
                                          Context lc, Context rc) const
{
	Arity lhs_arity(lhs.size());
	Arity rhs_arity(rhs.size());
	OC_ASSERT(lhs_arity == rhs_arity);

	// Base case
	if (lhs_arity == 0)
		return SolutionSet(true);

	// Recursive case
	SolutionSet sol;
	for (Arity i = 0; i < lhs_arity; ++i) {
		auto head_sol = unify(lhs[i], rhs[0], lc, rc);
		if (head_sol.is_satisfiable()) {
			HandleSeq lhs_tail(cp_erase(lhs, i));
			HandleSeq rhs_tail(cp_erase(rhs, 0));
			auto tail_sol = unordered_unify(lhs_tail, rhs_tail, lc, rc);
			SolutionSet perm_sol = join(head_sol, tail_sol);
			// Union merge satisfiable permutations
			sol.insert(perm_sol.begin(), perm_sol.end());
		}
	}
	return sol;
}

Unify::SolutionSet Unify::ordered_unify(const HandleSeq& lhs,
                                        const HandleSeq& rhs,
                                        Context lc, Context rc) const
{
	Arity lhs_arity(lhs.size());
	Arity rhs_arity(rhs.size());
	OC_ASSERT(lhs_arity == rhs_arity);

	SolutionSet sol(true);
	for (Arity i = 0; i < lhs_arity; ++i) {
		auto rs = unify(lhs[i], rhs[i], lc, rc);
		sol = join(sol, rs);
		if (not sol.is_satisfiable())     // Stop if unification has failed
			break;
	}
	return sol;
}

Unify::SolutionSet Unify::pairwise_unify(const std::set<CHandlePair>& pchs) const
{
	SolutionSet sol(true);
	for (const CHandlePair& pch : pchs) {
		auto rs = unify(pch.first, pch.second);
		sol = join(sol, rs);
		if (not sol.is_satisfiable())     // Stop if unification has failed
			return sol;
	}
	return sol;
}

Unify::SolutionSet Unify::comb_unify(const std::set<CHandle>& lhs,
                                     const std::set<CHandle>& rhs) const
{
	SolutionSet sol(true);
	for (const CHandle& lch : lhs) {
		for (const CHandle& rch : rhs) {
			auto rs = unify(lch, rch);
			sol = join(sol, rs);
			if (not sol.is_satisfiable())     // Stop if unification has failed
				return sol;
		}
	}
	return sol;
}

Unify::SolutionSet Unify::comb_unify(const std::set<CHandle>& chs) const
{
	SolutionSet sol(true);
	for (auto lit = chs.begin(); lit != chs.end(); ++lit) {
		for (auto rit = std::next(lit); rit != chs.end(); ++rit) {
			auto rs = unify(*lit, *rit);
			sol = join(sol, rs);
			if (not sol.is_satisfiable())     // Stop if unification has failed
				return sol;
		}
	}
	return sol;
}
	
bool Unify::is_unordered(const Handle& h) const
{
	return classserver().isA(h->get_type(), UNORDERED_LINK);
}

HandleSeq Unify::cp_erase(const HandleSeq& hs, Arity i) const
{
	HandleSeq hs_cp(hs);
	hs_cp.erase(hs_cp.begin() + i);
	return hs_cp;
}

Unify::SolutionSet Unify::mkvarsol(CHandle lch, CHandle rch) const
{
	// Attempt to consume quotation to avoid putting quoted elements
	// in the block.
	if (lch.is_free_variable() and rch.is_consumable() and rch.is_quoted())
		rch.update();
	if (rch.is_free_variable() and lch.is_consumable() and lch.is_quoted())
		lch.update();

	Handle inter = type_intersection(lch, rch);
	if (not inter)
		return SolutionSet();
	else {
		Block pblock{lch, rch};
		Partitions par{{{pblock, inter}}};
		return SolutionSet(par);
	}
}

Unify::SolutionSet Unify::join(const SolutionSet& lhs,
                               const SolutionSet& rhs) const
{
	// No need to join if one of them is non satisfiable
	if (not lhs.is_satisfiable() or not rhs.is_satisfiable())
		return SolutionSet();

	// By now both are satisfiable, thus non empty, join them
	SolutionSet result;
	for (const Partition& rp : rhs) {
		SolutionSet sol(join(lhs, rp));
		result.insert(sol.begin(), sol.end());
	}

	return result;
}

Unify::SolutionSet Unify::join(const SolutionSet& lhs, const Partition& rhs) const
{
	// Base cases
	if (rhs.empty())
		return lhs;

	// Recursive case (a loop actually)
	SolutionSet result;
	for (const auto& par : lhs) {
		SolutionSet jps = join(par, rhs);
		result.insert(jps.begin(), jps.end());
	}
	return result;
}

Unify::SolutionSet Unify::join(const Partition& lhs, const Partition& rhs) const
{
	// Don't bother joining if lhs is empty (saves a bit of computation)
	if (lhs.empty())
		return SolutionSet({rhs});

	// Join
	SolutionSet result({lhs});
	for (const TypedBlock& rhs_block : rhs) {
		// For now we assume result has only 0 or 1 partition
		result = join(result, rhs_block);
		if (not result.is_satisfiable())
			return SolutionSet();
	}

	return result;
}

Unify::SolutionSet Unify::join(const SolutionSet& sol,
                               const TypedBlock& block) const
{
	SolutionSet result;
	for (const Partition& partition : sol) {
		SolutionSet jps = join(partition, block);
		result.insert(jps.begin(), jps.end());
	}
	return result;
}

Unify::SolutionSet Unify::join(const Partition& partition,
                               const TypedBlock& block) const
{
	// Find all partition blocks that have elements in common with block
	TypedBlockSeq common_blocks;
	for (const TypedBlock& p_block : partition)
		if (not has_empty_intersection(block.first, p_block.first))
			common_blocks.push_back(p_block);

	Partition jp(partition);
	if (common_blocks.empty()) {
		// If none then merely insert the independent block
		jp.insert(block);
		return SolutionSet({jp});
	} else {
		// Otherwise join block with all common blocks and replace
		// them by the result (if satisfiable, otherwise return the
		// empty solution set)
		TypedBlock j_block = join(common_blocks, block);
		if (is_satisfiable(j_block)) {
			for (const TypedBlock& rm : common_blocks)
				jp.erase(rm.first);
			jp.insert(j_block);

			// Perform the sub-unification of all common blocks with
			// block and join the solution set to jp
			SolutionSet sol = subunify(common_blocks, block);
			if (sol.is_satisfiable())
				return join(sol, jp);
		}
		return SolutionSet(false);
	}
}

Unify::TypedBlock Unify::join(const TypedBlockSeq& common_blocks,
                              const TypedBlock& block) const
{
	std::pair<Block, Handle> result{block};
	for (const auto& c_block : common_blocks) {
		result =  join(result, c_block);
        // Abort if unsatisfiable
        if (not is_satisfiable(result))
            return result;
    }
	return result;
}

Unify::TypedBlock Unify::join(const TypedBlock& lhs, const TypedBlock& rhs) const
{
    OC_ASSERT(lhs.second and rhs.second, "Can only join 2 satisfiable blocks");
	return {set_union(lhs.first, rhs.first),
			type_intersection(lhs.second, rhs.second)};
}

Unify::SolutionSet Unify::subunify(const TypedBlockSeq& common_blocks,
                                   const TypedBlock& block) const
{
	// Form a set with all terms
	std::set<CHandle> all_chs(block.first);
	for (const TypedBlock& cb : common_blocks)
		all_chs.insert(cb.first.begin(), cb.first.end());

	// Build a set of all pairs of terms that may have not been
	// unified so far.
	std::set<CHandlePair> not_unified;
	// This function returns true iff both terms are in the given
	// block. If so it means they have already been unified.
	auto both_in_block = [](const CHandle& lch, const CHandle& rch,
	                        const TypedBlock& block) {
		return is_in(lch, block.first) and is_in(rch, block.first);
	};
	for (auto lit = all_chs.begin(); lit != all_chs.end(); ++lit) {
		for (auto rit = std::next(lit); rit != all_chs.end(); ++rit) {
			// Check if they are in block
			bool already_unified = both_in_block(*lit, *rit, block);
			// If not, then check if they are in one of the common
			// blocks
			if (not already_unified) {
				for (const TypedBlock& cb : common_blocks) {
					already_unified = both_in_block(*lit, *rit, cb);
					if (already_unified)
						break;
				}
			}
			if (not already_unified)
				not_unified.insert({*lit, *rit});
		}
	}

	// Unify all not unified yet terms
	return pairwise_unify(not_unified);
}

Unify::SolutionSet Unify::subunify(const TypedBlock& lhs,
                                   const TypedBlock& rhs) const
{
	return comb_unify(set_symmetric_difference(lhs.first, rhs.first));
}

bool Unify::is_satisfiable(const TypedBlock& block) const
{
	return (bool)block.second;
}

bool hm_content_eq(const HandleMap& lhs, const HandleMap& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin();
	auto rit = rhs.begin();
	while (lit != lhs.end()) {
		if (not content_eq(lit->first, rit->first)
		   or not content_eq(lit->second, rit->second))
			return false;
		++lit; ++rit;
	}
	return true;
}

bool hchm_content_eq(const Unify::HandleCHandleMap& lhs,
                     const Unify::HandleCHandleMap& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin();
	auto rit = rhs.begin();
	while (lit != lhs.end()) {
		if (not content_eq(lit->first, rit->first)
		    or lit->second != rit->second)
			return false;
		++lit; ++rit;
	}
	return true;
}

bool ts_content_eq(const Unify::TypedSubstitution& lhs,
                   const Unify::TypedSubstitution& rhs)
{
	return lhs.first.size() == rhs.first.size()
		and hchm_content_eq(lhs.first, rhs.first)
		and content_eq(lhs.second, rhs.second);
}

bool tss_content_eq(const Unify::TypedSubstitutions& lhs,
                    const Unify::TypedSubstitutions& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin();
	auto rit = rhs.begin();
	while (lit != lhs.end()) {
		if (not ts_content_eq(*lit, *rit))
			return false;
		++lit; ++rit;
	}
	return true;
}

HandleMap strip_context(const Unify::HandleCHandleMap& hchm)
{
	HandleMap result;
	for (auto& el : hchm)
		result.insert({el.first, el.second.handle});
	return result;
}

/**
 * Generate a VariableList of the free variables of a given contextual
 * atom ch.
 */
VariableListPtr gen_varlist(const Unify::CHandle& ch)
{
	HandleSet free_vars = ch.get_free_variables();
	return createVariableList(HandleSeq(free_vars.begin(), free_vars.end()));
}

Handle Unify::type_intersection(const CHandle& lch, const CHandle& rch) const
{
	return type_intersection(lch.handle, rch.handle, lch.context, rch.context);
}

Handle Unify::type_intersection(const Handle& lh, const Handle& rh,
                                Context lc, Context rc) const
{
	if (inherit(lh, rh, lc, rc))
		return lh;
	if (inherit(rh, lh, rc, lc))
		return rh;
	return Handle::UNDEFINED;
}

std::set<Type> Unify::simplify_type_union(std::set<Type>& type) const
{
	return {}; // TODO: do we really need that?
}

std::set<Type> Unify::get_union_type(const Handle& h) const
{
	const VariableTypeMap& vtm = _variables._simple_typemap;
	auto it = vtm.find(h);
	if (it == vtm.end() or it->second.empty())
		return {ATOM};
	else {
		return it->second;
	}
}

bool Unify::inherit(const CHandle& lch, const CHandle& rch) const
{
	return inherit(lch.handle, rch.handle, lch.context, rch.context);
}

bool Unify::inherit(const Handle& lh, const Handle& rh,
                    Context lc, Context rc) const
{
	Type lt = lh->get_type();
	Type rt = rh->get_type();

	// Recursive cases

	// Consume quotations
	if (lc.quotation.consumable(lt)) {
		lc.quotation.update(lt);
		return inherit(lh->getOutgoingAtom(0), rh, lc, rc);
	}
	if (rc.quotation.consumable(rt)) {
		rc.quotation.update(rt);
		return inherit(lh, rh->getOutgoingAtom(0), lc, rc);
	}

	// If both are links then check that the outgoings of lhs inherit
	// the outgoings of rhs.
	if (lh->is_link() and rh->is_link() and (lt == rt)) {
		if (lh->get_arity() == rh->get_arity()) {
			for (size_t i = 0; i < lh->get_arity(); i++) {
				if (not inherit(lh->getOutgoingAtom(i),
				                rh->getOutgoingAtom(i),
				                lc, rc))
					return false;
			}
			return true;
		} else return false;
	}

	// Base cases

	// If they are equal then lh trivial inherits from rh
	if (lh == rh)
		return true;

	// If both are unquoted variables then look at then types (only
	// simple types are considered for now).
	if (lc.quotation.is_unquoted() and VARIABLE_NODE == lt
	    and rc.quotation.is_unquoted() and VARIABLE_NODE == rt)
		return inherit(get_union_type(lh), get_union_type(rh));

	// If only rh is a variable, if its in _variable then check
	// whether lh type inherits from it (using Variables::is_type),
	// otherwise assume rh is the top type and thus anything inherits
	// from it.
	if (rc.quotation.is_unquoted() and VARIABLE_NODE == rt)
        return not _variables.is_in_varset(rh) or _variables.is_type(rh, lh);

	return false;
}

bool Unify::inherit(Type lhs, Type rhs) const
{
	return classserver().isA(lhs, rhs);
}

bool Unify::inherit(Type lhs, const std::set<Type>& rhs) const
{
	for (Type ty : rhs)
		if (inherit(lhs, ty))
			return true;
	return false;
}

bool Unify::inherit(const std::set<Type>& lhs, const std::set<Type>& rhs) const
{
	for (Type ty : lhs)
		if (not inherit(ty, rhs))
			return false;
	return true;
}

Variables merge_variables(const Variables& lhs, const Variables& rhs)
{
	Variables new_vars(lhs);
	new_vars.extend(rhs);
	return new_vars;
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

	Variables new_vars =
		merge_variables(lhs_vl.get_variables(), rhs_vl.get_variables());
	return new_vars.get_vardecl();
}

std::string oc_to_string(const Unify::CHandle& ch)
{
	std::stringstream ss;
	ss << "context:" << std::endl << oc_to_string(ch.context)
	   << "atom:" << std::endl << oc_to_string(ch.handle);
	return ss.str();
}

std::string oc_to_string(const Unify::Block& pb)
{
	std::stringstream ss;
	ss << "size = " << pb.size() << std::endl;
	int i = 0;
	for (const auto& el : pb)
		ss << "catom[" << i++ << "]:" << std::endl << oc_to_string(el);
	return ss.str();
}

std::string oc_to_string(const Unify::TypedBlock& tb)
{
	std::stringstream ss;
	ss << "block:" << std::endl << oc_to_string(tb.first)
	   << "type:" << std::endl << oc_to_string(tb.second);
	return ss.str();
}

std::string oc_to_string(const Unify::TypedBlockSeq& tbs)
{
	std::stringstream ss;
	ss << "size = " << tbs.size() << std::endl;
	for (size_t i = 0; i < tbs.size(); i++)
		ss << "typed block[" << i << "]:" << std::endl
		   << oc_to_string(tbs[i]);
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

std::string oc_to_string(const Unify::HandleCHandleMap& hchm)
{
	std::stringstream ss;
	ss << "size = " << hchm.size() << std::endl;
	int i = 0;
	for (const auto& hch : hchm) {
		ss << "atom[" << i << "]:" << std::endl
		   << oc_to_string(hch.first);
		ss << "catom[" << i << "]:" << std::endl
		   << oc_to_string(hch.second);
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

} // namespace opencog
