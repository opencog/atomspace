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

#include <opencog/util/random.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>

#include "PrenexLink.h"

using namespace opencog;

void PrenexLink::init(void)
{
	Type t = get_type();
	if (PRENEX_LINK == t)
		throw InvalidParamException(TRACE_INFO,
			"PrenexLinks are private and cannot be instantiated.");
	if (not nameserver().isA(t, PRENEX_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a PrenexLink, got %s", tname.c_str());
	}
}

PrenexLink::PrenexLink(const Handle& vars, const Handle& body)
	: RewriteLink(HandleSeq({vars, body}), PRENEX_LINK)
{
	init();
}

PrenexLink::PrenexLink(const HandleSeq&& oset, Type t)
	: RewriteLink(std::move(oset), t)
{
	if (skip_init(t)) return;
	init();
}

/* ================================================================= */

/// Re-assemble into prenex form.
///
/// If the result of beta reduction is an expression with bound
/// variables in it, then those bound variables should be moved
/// to the outermost link, viz, be put into prenex form. All of
/// the analysis of the term has already happened; here, we just
/// need to assemble the final prenex form.
//
Handle PrenexLink::reassemble(Type prenex,
                              const HandleMap& vm,
                              const Variables& final_variables) const
{
	// Now get the vardecl and body
	Handle vdecl = final_variables.get_vardecl();
	Handle newbod = RewriteLink::substitute_body(vdecl, _body, vm);

	// Reassemble if necessary. That is, if there are variables to
	// declare, place them outermost, in prenex form. We only
	// re-assemble into prenex form if the desired link type actually
	// is a prenex link type. If it's not, then it should not get
	// prenexed.  Check for PutLink to avoid infinite recursion.
	if (PUT_LINK != prenex and not final_variables.empty() and
	    nameserver().isA(prenex, PRENEX_LINK))
	{
		return Handle(createLink(prenex, vdecl, newbod));
	}

	// Otherwise, we are done with the beta-reduction.
	return newbod;
}

/* ================================================================= */

// Collect up variables.
static Handle collect(const Variables& vtool,
                      const Handle& origvar,
                      const Handle& newvar,
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
		alt = createNode(VARIABLE_NODE, std::move(altname));
	} while (used_vars.find(alt) != used_vars.end());

	final_varlist.emplace_back(vtool.get_type_decl(origvar, alt));
	used_vars.insert(alt);
	issued_vars.insert({newvar, alt});
	return alt;
}

/* ================================================================= */

Handle PrenexLink::beta_reduce(const HandleSeq& seq) const
{
	// Test for a special case: function composition followed by
	// eta conversion on the supplied function.  We can recognize
	// this if we get fewer arguments than expected.
	const Variables& vtool = get_variables();
	size_t seqsize = seq.size();
	if (seqsize == vtool.size())
	{
		// Not an eta reduction. Do the normal thing.
		return RewriteLink::beta_reduce(seq);
	}

	// If we are here, we are expecting an eta conversion.
	// Here is what an eta conversion looks like (see test_eta in
	// PutLinkUTest):
	//
	// (define func-with-three-args
	//    (Lambda
	//      (VariableList (Variable "$x")(Variable "$y")(Variable "$z"))
	//      (Inheritance (Variable "$z") (Variable "$x"))))
	//
	// The above is a function that expects three arguments: x,y,z.
	// Lets compose it with a function that takes one argument, but
	// returns three results (function composition):
	//
	// (define func-returns-three-results
	//    (Lambda (Variable "$w")
	//      (List (Concept "animal") (Concept "foobar") (Variable"$w"))))
	//
	// Composing these two should return a function that takes one
	// argument, namely $w.
	//
	//    (Put func-with-three-args func-returns-three-results)
	//
	// We expect as a result:
	//
	// (Lambda (Variable "$w") (Inheritance (Variable "$w") (Concept "animal")))
	//
	// Note that this is NOT compatible with beta-reduction in classical
	// lambda calculus, which would leave $w free. We really want to
	// eta-convert this, and keep $w bound, so that it looks like
	// ordinary function composition. Atomese is not lambda calculus!

	// ------- Lets begin.
	// For a valid eta conversion, there must be just one argument,
	// and it must must be a ScopeLink.
	if (1 != seqsize or
	    not nameserver().isA(seq[0]->get_type(), SCOPE_LINK))
	{
		if (_silent) throw TypeCheckException();
		throw SyntaxException(TRACE_INFO,
		   "PrenexLink is badly formed");
	}

	// If its an eta, it had better have the right size.
	ScopeLinkPtr lam(ScopeLinkCast(seq[0]));
	const Handle& body = lam->get_body();
	if (body->get_type() != LIST_LINK or
	    body->get_arity() != vtool.size())
	{
		if (_silent) throw TypeCheckException();
		throw SyntaxException(TRACE_INFO,
		   "PrenexLink has mismatched eta, expecting %lu == %lu",
		   vtool.size(), body->get_arity());
	}

	// If we are here, we have a valid eta reduction to perform.
	HandleSeq final_varlist;
	HandleSet used_vars;
	HandleMap issued;

	// First, figure out what the new variables will be.
	//
	// TODO: this step could be simplified by using final_variables
	// instead of final_varlist and modifying collect to process the
	// whole bound at once, with the help of Variables::extend.
	// Variables::extend should also be modified to return the mapping
	// between old and new variables when alpha-conversion is used.
	// Similar simplications can be applied to all calls of collect.
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

	// Whether the final_varlist should be ordered. If any variable
	// declaration involved in the beta-reduction is ordered then it
	// should be.
	bool final_ordered = vtool._ordered or bound._ordered;

	// Almost done. The final_varlist holds the variable declarations,
	// and the vm holds what needs to be substituted in. Substitute,
	// and create the reduced link.
	return reassemble(get_type(), vm, Variables(final_varlist, final_ordered));
}

/* ================================================================= */

Handle PrenexLink::beta_reduce(const HandleMap& vmap) const
{
	HandleMap vm = vmap;

	// If any of the mapped arguments are ScopeLinks, we need to discover
	// and collect up the variables that they bind. We also need to
	// make sure that they are "fresh", i.e. don't have naming
	// collisions.
	HandleSeq final_varlist;
	HandleSet used_vars;
	HandleMap issued;
	Type prenex = get_type();

	const Variables& vtool = get_variables();

	// Whether the final_varlist should be ordered. If any variable
	// declaration involved in the beta-reduction is ordered then it
	// should be.
	bool final_ordered = vtool._ordered;

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

		Type argtype = pare->second->get_type();

		// If we are here, then var will be beta-reduced.
		// But if the argument is another variable, then
		// alpha-convert, instead.
		if (VARIABLE_NODE == argtype)
		{
			Handle alt = collect(vtool, var, pare->second,
			                     final_varlist, used_vars, issued);
			if (alt)
				vm[var] = alt;
			continue;
		}

		// If we are here, then var will be beta-reduced.
		// Is the argument a PrenexLink? If so, we need to disassmeble
		// it, yank out the variables, and then reassemble it
		// again, so that the variables are declared in the
		// outer-most scope.
		if (nameserver().isA(argtype, PRENEX_LINK))
		{
			PrenexLinkPtr sc = PrenexLinkCast(pare->second);
			const Variables& bound = sc->get_variables();
			final_ordered = final_ordered or bound._ordered;
			Handle body = sc->get_body();

			// The body might not exist, if there's an unmantched
			// UnquoteLink in it. In such a case the beta-reduction is
			// aborted.
			if (nullptr == body)
			{
				throw SyntaxException(TRACE_INFO,
				                      "PrenexLink given a malformed argument=%s",
				                      sc->to_string().c_str());
			}

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

			// Last one wins.  XXX This is actually ambiguous, if there
			// were multiple variables, and they weren's all LambdaLinks,
			// for example. In that case, things are borked, and there's
			// a bug here.  For now, we punt.
			prenex = argtype;
		}
	}

	// Almost done. The final_varlist holds the variable declarations,
	// and the vm holds what needs to be substituted in. Substitute,
	// and create the reduced link.
	return reassemble(prenex, vm, Variables(final_varlist, final_ordered));
}

/* ================================================================= */

DEFINE_LINK_FACTORY(PrenexLink, PRENEX_LINK);

/* ===================== END OF FILE ===================== */
