/*
 * PrenexLink.cc
 *
 * Copyright (C) 2017 Linas Vepstas
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

#include <opencog/util/mt19937ar.h>
#include <opencog/util/random.h>
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atomutils/TypeUtils.h>
#include <opencog/atomutils/FindUtils.h>

#include "PrenexLink.h"

using namespace opencog;

void PrenexLink::init(void)
{
	Type t = get_type();
	if (not classserver().isA(t, PRENEX_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a PrenexLink, got %s", tname.c_str());
	}
}

PrenexLink::PrenexLink(const Handle& vars, const Handle& body)
	: RewriteLink(HandleSeq({vars, body}), PRENEX_LINK)
{
	init();
}

PrenexLink::PrenexLink(const HandleSeq& oset, Type t)
	: RewriteLink(oset, t)
{
	if (skip_init(t)) return;
	init();
}

PrenexLink::PrenexLink(const Link &l)
	: RewriteLink(l)
{
	if (skip_init(l.get_type())) return;
	init();
}

/* ================================================================= */

Handle PrenexLink::reassemble(const HandleMap& vm,
                              const HandleSeq& final_varlist) const
{
	const Variables& vtool = get_variables();

	// Now get the new body...
	Handle newbod = vtool.substitute(_body, vm, _silent);

	if (0 < final_varlist.size())
	{
		Handle vdecl;
		if (1 == final_varlist.size())
			vdecl = final_varlist[0];
		else
			vdecl = Handle(createVariableList(final_varlist));

		return Handle(createLink(get_type(), vdecl, newbod));
	}

	return newbod;
}

/* ================================================================= */

static Handle collect(const Variables& vtool,
                      const Handle& origvar, const Handle& newvar,
                      HandleSeq& final_varlist,
                      HandleSet& used_vars,
                      HandleMap& issued_vars)
{
	// If we've already issued this variable, do not re-issue it.
	const auto& pr = issued_vars.find(newvar);
	if (pr != issued_vars.end())
		return pr->second;

	// Is there a naming collision?
	if (used_vars.find(newvar) == used_vars.end())
	{
		final_varlist.emplace_back(vtool.get_type_decl(origvar, newvar));
		used_vars.insert(newvar);
		issued_vars.insert({newvar, newvar});
		return Handle::UNDEFINED;
	}

	// Aiiee, there is a collision, make a new name!
	Handle alt;
	do
	{
		std::string altname = randstr(newvar->get_name() + "-");
		alt = createNode(VARIABLE_NODE, altname);
	} while (used_vars.find(alt) != used_vars.end());

	final_varlist.emplace_back(vtool.get_type_decl(origvar, alt));
	used_vars.insert(alt);
	issued_vars.insert({newvar, alt});
	return alt;
}

/* ================================================================= */

Handle PrenexLink::beta_reduce(const HandleSeq& seq) const
{
	// Test for a special case: eta reduction on the supplied
	// function.  We can recognize this if we don't get fewer
	// arguments than we expected.
	const Variables& vtool = get_variables();
	size_t seqsize = seq.size();
	if (seqsize == vtool.size())
	{
		// Not an eta reduction. Do the normal thing.
		return RewriteLink::beta_reduce(seq);
	}

	// If its an eta, there must be just one argument, and it must
	// must be a ScopeLink.
	if (1 != seqsize or
	    not classserver().isA(seq[0]->get_type(), SCOPE_LINK))
	{
		if (_silent) return Handle::UNDEFINED;
		throw SyntaxException(TRACE_INFO,
		   "PrenexLink is badly formed");
	}

	// If its an eta, it had better have the right size.
	ScopeLinkPtr lam(ScopeLinkCast(seq[0]));
	const Handle& body = lam->get_body();
	if (body->get_arity() != vtool.size() or
	    body->get_type() != LIST_LINK)
	{
		if (_silent) return Handle::UNDEFINED;
		throw SyntaxException(TRACE_INFO,
		   "PrenexLink has mismatched eta, expecting %lu == %lu",
		   vtool.size(), body->get_arity());
	}

	// If we are here, we have a valid eta reduction to perform.
	HandleSeq final_varlist;
	HandleSet used_vars;
	HandleMap issued;

	// First, figure out what the new variables will be.
	Variables bound = lam->get_variables();
	for (const Handle& bv: bound.varseq)
	{
		collect(bound, bv, bv, final_varlist, used_vars, issued);
	}

	// Next, figure out what substitutions will be made.
	HandleMap vm;
	const HandleSeq& oset = body->getOutgoingSet();
	for (size_t i=0; i<vtool.size(); i++)
	{
		vm.insert({vtool.varseq[i], oset[i]});
	}

	// Almost done. The final_varlist holds the variable declarations,
	// and the vm holds what needs to be substituted in. Substitute,
	// and create the reduced link.
	return reassemble(vm, final_varlist);
}

/* ================================================================= */

Handle PrenexLink::beta_reduce(const HandleMap& vmap) const
{
	HandleMap vm = vmap;

	// If any of the mapped values are ScopeLinks, we need to discover
	// and collect up the variables that they bind. We also need to
	// make sure that they are "fresh", i.e. don't have naming
	// collisions.
	HandleSeq final_varlist;
	HandleSet used_vars;
	HandleMap issued;

	Variables vtool = get_variables();
	for (const Handle& var : vtool.varseq)
	{
		// If we are not substituting for this variable, copy it
		// over to the final list.
		const auto& pare = vm.find(var);
		if (vm.find(var) == vm.end())
		{
			Handle alt = collect(vtool, var, var,
			                     final_varlist, used_vars, issued);
			if (alt)
				vm.insert({var, alt});
			continue;
		}

		Type valuetype = pare->second->get_type();

		// If we are here, then var will be beta-reduced.
		// But if the value is another variable, then alpha-convert,
		// instead.
		if (VARIABLE_NODE == valuetype)
		{
			Handle alt = collect(vtool, var, pare->second,
			                     final_varlist, used_vars, issued);
			if (alt)
				vm[var] = alt;
			continue;
		}

		// If we are here, then var will be beta-reduced.
		// Is the value a ScopeLink? If so, handle it.
		if (classserver().isA(valuetype, SCOPE_LINK))
		{
			ScopeLinkPtr sc = ScopeLinkCast(pare->second);
			Variables bound = sc->get_variables();
			Handle body = sc->get_body();
			HandleMap scopissued;
			for (const Handle& bv : bound.varseq)
			{
				Handle alt = collect(bound, bv, bv,
				                     final_varlist, used_vars, scopissued);
				if (alt)
				{
					// In the body of the scope link, rename
					// the bond variable to its new name.
					HandleMap alpha;
					alpha[bv] = alt;
					body = bound.substitute_nocheck(body, alpha);
				}
			}
			vm[pare->first] = body;
		}
	}

	// Almost done. The final_varlist holds the variable declarations,
	// and the vm holds what needs to be substituted in. Substitute,
	// and create the reduced link.
	return reassemble(vm, final_varlist);
}

/* ================================================================= */

DEFINE_LINK_FACTORY(PrenexLink, PRENEX_LINK);

/* ===================== END OF FILE ===================== */
