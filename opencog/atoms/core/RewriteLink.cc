/*
 * RewriteLink.cc
 *
 * Copyright (C) 2017 Nil Geisweiller
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <string>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>

#include "LambdaLink.h"
#include "RewriteLink.h"

using namespace opencog;

void RewriteLink::init(void)
{
	Type t = get_type();

#if 0
	// The unit tests create this directly, so this check bombs.
	if (REWRITE_LINK == t)
		throw InvalidParamException(TRACE_INFO,
			"RewriteLinks are private and cannot be instantiated.");
#endif
	if (not nameserver().isA(t, REWRITE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a RewriteLink, got %s", tname.c_str());
	}
}

RewriteLink::RewriteLink(const Handle& vars, const Handle& body)
	: ScopeLink(HandleSeq({vars, body}), REWRITE_LINK), _silent(false)
{
	init();
}

RewriteLink::RewriteLink(const HandleSeq&& oset, Type t)
	: ScopeLink(std::move(oset), t), _silent(false)
{
	if (skip_init(t)) return;
	init();
}

/* ================================================================= */

Handle RewriteLink::beta_reduce(const HandleMap& vm) const
{
	// Perform substitution over the variable declaration
	Handle nvardecl = substitute_vardecl(vm);

	// Perform substitution over the bodies
	HandleSeq hs = substitute_bodies(nvardecl, vm);

	// Filter vardecl
	nvardecl = filter_vardecl(nvardecl, hs);

	// Insert vardecl in the outgoing set, if defined
	if (nvardecl)
	{
		hs.insert(hs.begin(), nvardecl);
	}
	else
	{
		// Its illegal to create a PutLink that is not in the
		// form of a redex, i.e. doesn't have variable declarations
		// in it. So we must not call createLink(), below.
		Type t = get_type();
		if (PUT_LINK == t or LAMBDA_LINK == t)
			return hs.at(0);
	}

	// Create the substituted scope.  I suspect that this is a bad
	// idea, when nvardecl==nullptr, I mean, its just gonna be weird,
	// and cause issues thhrought the code ... but ... whatever.
	return createLink(std::move(hs), get_type());
}

Handle RewriteLink::beta_reduce(const HandleSeq& vals) const
{
	const Variables& vars = get_variables();
	if (vals.size() != vars.size())
	{
		if (1 != vals.size() or LAMBDA_LINK != vals[0]->get_type())
		{
			if (_silent) throw TypeCheckException();

			throw SyntaxException(TRACE_INFO,
				"RewriteLink has mismatched arity, expecting %lu == %lu",
				vars.size(), vals.size());
		}

		// Verify that eta reduction is possible...
		LambdaLinkPtr lam(LambdaLinkCast(vals[0]));
		const Handle& body = lam->get_body();

		if (body->get_arity() != vars.size() or
		    body->get_type() != LIST_LINK)
		{
			if (_silent) throw TypeCheckException();

			throw SyntaxException(TRACE_INFO,
				"RewriteLink has mismatched eta, expecting %lu == %lu",
				vars.size(), body->get_arity());
		}

		// Perform a very simple-minded eta reduction.
		// This will be incorrect under a variety of corner cases,
		// but gives approximately correct results for the simple case.
		const HandleSeq& eta = body->getOutgoingSet();
		HandleMap vm;

		for (size_t i=0; i<eta.size(); i++)
			vm.insert({vars.varseq[i], eta[i]});
		return beta_reduce(vm);
	}

	HandleMap vm;
	for (size_t i=0; i<vals.size(); i++)
	{
		vm.insert({vars.varseq[i], vals[i]});
	}

	// Call the possibly-overloaded map-based reduction function.
	return beta_reduce(vm);
}

HandleSeq RewriteLink::substitute_bodies(const Handle& nvardecl,
                                         const HandleMap& vm) const
{
	return beta_reduce_bodies(nvardecl, vm);
}

HandleSeq RewriteLink::beta_reduce_bodies(const Handle& nvardecl,
                                          const HandleMap& vm) const
{
	HandleSeq hs;
	for (size_t i = (get_vardecl() ? 1 : 0); i < get_arity(); ++i)
	{
		const Handle& h = getOutgoingAtom(i);
		hs.push_back(substitute_body(nvardecl, h, vm));
	}
	return hs;
}

Handle RewriteLink::substitute_body(const Handle& nvardecl,
                                    const Handle& body,
                                    const HandleMap& vm) const
{
	Handle nbody = get_variables().substitute(body, vm, _silent);

	Variables vars(nvardecl);
	bool needless_quotation = true;
	nbody = consume_quotations(vars, nbody, Quotation(),
	                           needless_quotation, true);
	return nbody;
}

Handle RewriteLink::substitute_vardecl(const HandleMap& vm) const
{
	if (not get_vardecl())
		return Handle::UNDEFINED;

	return substitute_vardecl(get_vardecl(), vm);
}

Handle RewriteLink::substitute_vardecl(const Handle& vardecl,
                                       const HandleMap& vm)
{
	Type t = vardecl->get_type();

	// Base cases

	if (t == VARIABLE_NODE)
	{
		auto it = vm.find(vardecl);

		// Only substitute if the variable is substituted by another variable
		if (it == vm.end())
			return vardecl;
		if (it->second->get_type() == VARIABLE_NODE)
			return it->second;
		return Handle::UNDEFINED;
	}

	// Recursive cases

	HandleSeq oset;

	if (t == VARIABLE_LIST)
	{
		for (const Handle& h : vardecl->getOutgoingSet())
		{
			Handle nh = substitute_vardecl(h, vm);
			if (nh)
				oset.push_back(nh);
		}
		if (oset.empty())
			return Handle::UNDEFINED;
	}
	else if (t == TYPED_VARIABLE_LINK)
	{
		Handle new_var = substitute_vardecl(vardecl->getOutgoingAtom(0), vm);
		if (new_var)
		{
			oset.push_back(new_var);
			oset.push_back(vardecl->getOutgoingAtom(1));
		}
		else
			return Handle::UNDEFINED;
	}
	else
	{
		OC_ASSERT(false, "Not implemented");
	}
	return createLink(std::move(oset), t);
}

Handle RewriteLink::consume_quotations() const
{
	Handle vardecl = get_vardecl();
	const Variables& variables = get_variables();
	HandleSeq nouts;
	for (size_t i = (get_vardecl() ? 1 : 0); i < get_arity(); ++i)
	{
		bool clause_root = (i == (get_vardecl() ? 1 : 0));
		bool needless_quotation = true;
		Handle nbody = consume_quotations(variables, getOutgoingAtom(i),
		                Quotation(), needless_quotation, clause_root);
		nouts.push_back(nbody);
		// If the new body has terms with free variables but no
		// vardecl it means that some quotations are missing. Rather
		// than adding them we set vardecl to an empty VariableList.
		if (not vardecl and not get_free_variables(nbody).empty())
			vardecl = HandleCast(createVariableList(HandleSeq{}));
	}

	if (vardecl)
		nouts.insert(nouts.begin(), vardecl);

	// Recreate the scope
	return createLink(std::move(nouts), get_type());
}

Handle RewriteLink::consume_quotations(const Variables& variables,
                                       const Handle& h,
                                       Quotation quotation,
                                       bool& needless_quotation,
                                       bool clause_root)
{
	Type t = h->get_type();

	// Base case
	if (h->is_node())
	{
		// TODO: the following has no unit test!!! Yet it introduces a
		// bug covered by RewriteLinkUTest::test_consume_quotations_4(),
		// thus this code is disabled till a unit test it created for it
		// and we understand what it fixes and how it fixes.
		//
		// // Make sure quotation is not removed around
		// // GroundedPredicateNode as it otherwise changes the pattern
		// // matcher semantics as it will consider those as virtual.
		// if (t == GROUNDED_PREDICATE_NODE)
		// 	needless_quotation = false;

		return h;
	}

	// Recursive cases

	// Remember current quotation before updating it
	Quotation quotation_cp(quotation);
	quotation.update(t);

	if (quotation_cp.consumable(t))
	{
		Handle child = h->getOutgoingAtom(0);

		// A consumable quotation over a closed term not containing
		// any quotation or special executable links is clearly
		// useless, regardless of whether it has been determined
		// needless or not.
		if (not contains_atomtype(child, UNQUOTE_LINK) and
		    not contains_atomtype(child, PUT_LINK) and
		    not contains_atomtype(child, QUOTE_LINK) and
		    not contains_atomtype(child, LOCAL_QUOTE_LINK) and
		    is_closed(child))
		{
			return consume_quotations(variables, child,
			                          quotation, needless_quotation,
			                          clause_root);
		}

		if (t == UNQUOTE_LINK)
		{
			// A succession of (Unquote (Quote ..)) is an involution
			// and thus can be remove.
			//
			// TODO: generalize with when Unquote and Quote are apart
			if (child->get_type() == QUOTE_LINK)
			{
				quotation.update(child->get_type());
				return consume_quotations(variables, child->getOutgoingAtom(0),
				                          quotation, needless_quotation,
				                          clause_root);
			}

			// If it's been labelled as needless somewhere above, then
			// consume it. Otherwise don't consume it and reset
			// needless_quotation to true before the recursion.
			if (needless_quotation)
			{
				return consume_quotations(variables, child,
				                          quotation, needless_quotation,
				                          clause_root);
			}
			else
			{
				needless_quotation = true;
				Handle con_h = consume_quotations_mere_rec(variables, h,
				                                           quotation,
				                                           needless_quotation,
				                                           clause_root);
				needless_quotation = false;
				return con_h;
			}
		}

		if (t == QUOTE_LINK or t == LOCAL_QUOTE_LINK)
		{
			Handle con_child = consume_quotations(variables, child,
			                                      quotation,
			                                      needless_quotation,
			                                      clause_root);

			// Only consume if the quotation turned out to be needless
			if (needless_quotation)
				return con_child;

			// Otherwise keep the quotation, and reset
			// needless_quotation to true for the remaining tree
			needless_quotation = true;
			Handle new_con_child = createLink(t, con_child);
			new_con_child->copyValues(con_child);
			return new_con_child;
		}
	}

	// Scope or special links that may require not to consume
	// quotations as they may otherwise change the semantics
	bool need_quotation = (quotation_cp.is_quoted() and
	                       (t == PUT_LINK or
	                        nameserver().isA(t, FUNCTION_LINK) or
	                        nameserver().isA(t, EVALUATION_LINK) or
	                        (is_logical_connector(t) and
	                         quotation_cp.is_locally_quoted() and
	                         clause_root) or
	                        is_scope_bound_to_ancestor(variables, h)));

	// Propagate the need for quotation down
	if (need_quotation and not quotation_cp.is_locally_quoted())
		needless_quotation = false;

	// Remember that the children are potentially clause root
	clause_root = nameserver().isA(t, SCOPE_LINK);

	// Mere recursive call
	Handle ch = consume_quotations_mere_rec(variables, h,
	                                        quotation, needless_quotation,
	                                        clause_root);

	// Propagate the need for quotation up, because in case it is
	// local, we did not propagate it down.
	if (need_quotation and quotation_cp.is_locally_quoted())
		needless_quotation = false;

	return ch;
}

HandleSeq RewriteLink::consume_quotations(const Variables& vars,
                                          const HandleSeq& hs,
                                          Quotation quotation,
                                          bool& needless_qtn,
                                          bool clause_root)
{
	HandleSeq con_hs(hs.size());
	std::transform(hs.begin(), hs.end(), con_hs.begin(), [&](const Handle& h) {
			return consume_quotations(vars, h, quotation,
			                          needless_qtn, clause_root); });
	return con_hs;
}

Handle RewriteLink::consume_quotations_mere_rec(const Variables& variables,
                                                const Handle& h,
                                                Quotation quotation,
                                                bool& needless_quotation,
                                                bool clause_root)
{
	HandleSeq chs = consume_quotations(variables, h->getOutgoingSet(),
	                                   quotation, needless_quotation,
	                                   clause_root);
	Handle ch = createLink(std::move(chs), h->get_type());
	ch->copyValues(h);
	return ch;
}

bool RewriteLink::is_scope_bound_to_ancestor(const Variables& variables,
                                             const Handle& h)
{
	return nameserver().isA(h->get_type(), SCOPE_LINK) and
		is_bound_to_ancestor(variables, h);
}

bool RewriteLink::is_bound_to_ancestor(const Variables& variables,
                                       const Handle& local_scope)
{
	Handle unquote = local_scope->getOutgoingAtom(0);
	if (unquote->get_type() == UNQUOTE_LINK) {
		Handle vardecl = unquote->getOutgoingAtom(0);
		Type vdt = vardecl->get_type();
		if (vdt == VARIABLE_LIST or
		    vdt == VARIABLE_NODE or
		    vdt == TYPED_VARIABLE_LINK) {
			Variables local_vars = VariableList(vardecl).get_variables();
			return variables.varset_includes(local_vars.varset);
		}
	}
	return false;
}

bool RewriteLink::is_logical_connector(const Handle& h)
{
	return is_logical_connector(h->get_type());
}

bool RewriteLink::is_logical_connector(Type t)
{
	return t == AND_LINK or t == OR_LINK or t == NOT_LINK;
}

/* ================================================================= */

DEFINE_LINK_FACTORY(RewriteLink, REWRITE_LINK);

/* ===================== END OF FILE ===================== */
