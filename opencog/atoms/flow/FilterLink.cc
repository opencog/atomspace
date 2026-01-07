/*
 * opencog/atoms/flow/FilterLink.cc
 *
 * Copyright (C) 2015, 2016, 2022 Linas Vepstas
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/grant/DefineLink.h>
#include <opencog/atoms/scope/VariableSet.h>
#include <opencog/atoms/rule/RuleLink.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/ContainerValue.h>

#include "FilterLink.h"
#include "LinkSignatureLink.h"
#include "ValueShimLink.h"

using namespace opencog;

void FilterLink::init(void)
{
	_recursive_exec = false;

	// Filters consist of a function, and the data to apply the
	// function to.  The function can be explicit (inheriting from
	// ScopeLink) or implicit (we automatically fish out free variables).
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"FilterLink is expected to be arity-2 only!");

	Handle termpat = _outgoing[0];
	Type tscope = termpat->get_type();

	// Expand definitions
	if (nameserver().isA(tscope, DEFINED_PROCEDURE_NODE))
	{
		termpat = DefineLink::get_definition(termpat);
		if (nullptr == termpat)
			throw SyntaxException(TRACE_INFO,
				"FilterLink cannot find definition for %s",
				_outgoing[0]->to_string().c_str());

		tscope = termpat->get_type();
	}

	// First argument must be a function of some kind.
	// We convert all functions to guarded functions,
	// first, to manage the input-variables and the beta
	// reduction with ScopeLink, and second, to do
	// type-guarding with GuardLink.
	if (nameserver().isA(tscope, GUARD_LINK))
	{
		_guard_ptrn = GuardLinkCast(termpat);
	}
	else
	{
		FreeVariables fv;
		fv.find_variables(termpat);
		Handle decl(createVariableSet(std::move(fv.varseq)));
		_guard_ptrn = createGuardLink(HandleSeq{decl, termpat});
	}
	_mvars = &_guard_ptrn->get_variables();

	// RuleLinks are a special type of ScopeLink.  They specify a
	// re-write that should be performed.  Viz, RuleLinks are
	// of the form P(x)->Q(x).  Here, the `_rewrite` is the Q(x)
	if (nameserver().isA(tscope, RULE_LINK))
		_rewrite = RuleLinkCast(HandleCast(_guard_ptrn))->get_implicand();
}

FilterLink::FilterLink(const Handle& pattern, const Handle& term)
	: FunctionLink(HandleSeq({pattern, term}), FILTER_LINK)
{
	init();
}

FilterLink::FilterLink(Type t, const Handle& body)
	: FunctionLink(HandleSeq({body}), t)
{
	// Derived types have a different initialization sequence.
	if (FILTER_LINK != t) return;
	init();
}

FilterLink::FilterLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, FILTER_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw SyntaxException(TRACE_INFO,
			"Expecting a FilterLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (FILTER_LINK != t) return;
	init();
}

// ====================================================================

ValuePtr FilterLink::rewrite_one(const ValuePtr& vterm,
                                 AtomSpace* scratch, bool silent) const
{
	// See if the term passes pattern matching. If it does, the
	// side effect is that we get a grounding map as output.
	ValueMap valmap;
	if (not _guard_ptrn->guard(vterm, valmap, scratch, silent))
		return Handle::UNDEFINED;

	// Special case for signatures. The extract already rejected
	// mis-matches, if any. Thus, we are done, here.
	const Handle& body(_guard_ptrn->get_body());
	if (body->is_type(TYPE_NODE) or
	    (body->is_type(TYPE_OUTPUT_SIG) and
	       (not body->is_type(LINK_SIGNATURE_LINK))))
		return vterm;

	// If there is no RuleLink to fire, then just wrap up the results
	// and return them.
	if (_rewrite.empty())
	{
		if (1 == valmap.size()) return valmap.begin()->second;

		// Multiple Values to return. Two generic cases: the return
		// value is a set of Atoms, or a set of non-Atom Values.
		if (body->is_type(LINK_SIGNATURE_LINK))
		{
			ValueSeq valseq;
			for (const Handle& var : _mvars->varseq)
			{
				const auto& valpair = valmap.find(var);
				valseq.emplace_back(valpair->second);
			}
			return LinkSignatureLinkCast(body)->construct(scratch, std::move(valseq));
		}

		// A list of Handles.
		HandleSeq valseq;
		for (const Handle& var : _mvars->varseq)
		{
			const auto& valpair = valmap.find(var);
			if (valmap.end() == valmap.find(var))
				throw SyntaxException (TRACE_INFO,
					"Malformed FilterLink; no value for %s in %s",
					to_string().c_str(), var->to_string().c_str());

			valseq.emplace_back(HandleCast(valpair->second));
		}
		return scratch->add_link(LIST_LINK, std::move(valseq));
	}

	// If we are there, then there's a rule to fire. Two generic
	// cases to be handled:
	// 1) If the rewrite rule is not executable, then the grounding
	//    must consist of Atoms only. The grounding is plugged into
	//    the rewrite, i.e. beta-reduced.
	// 2) If the rewrite rule is executable, then the grounding
	//    is handed to that, without any further ado. The grounding
	//    might be either Values or Atoms; in either case, they're
	//    arguments for the function to be executed.

	// Place the groundings into a sequence, for easy access.
	HandleSeq valseq;
	for (const Handle& var : _mvars->varseq)
	{
		auto valpair = valmap.find(var);

		// This can happen in some unusual cases. So don't complain.
		if (valmap.end() == valpair)
			throw FatalErrorException(TRACE_INFO,
			     "Internal error; bug in filtering code");
			// continue;

		const ValuePtr& gnding(valpair->second);

		if (gnding->is_atom())
			valseq.emplace_back(HandleCast(gnding));
		else
			valseq.emplace_back(createValueShimLink(gnding));
	}

	ValueSeq rew;
	// Beta reduce, and execute. No type-checking during
	// beta-reduction; we've already done that, during matching.
	for (const Handle& impl : _rewrite)
	{
		ValuePtr red(_mvars->substitute_nocheck(impl, valseq, true));
		if (red->is_atom())
		{
			const Handle& hred(HandleCast(red));
			if (hred->is_executable())
			{
				ValuePtr v(hred->execute(scratch, silent));
				if (v)
				{
					if (v->is_atom())
						rew.emplace_back(scratch->add_atom(HandleCast(v)));
					else
						rew.emplace_back(v);
				}
			}
			else
				rew.emplace_back(scratch->add_atom(hred));
		}
		else
			rew.emplace_back(red);
	}

	if (1 == rew.size()) return rew[0];

	bool have_vals = false;
	for (const ValuePtr& v : rew)
		if (v and not v->is_atom()) { have_vals = true; break; }

	// Multiple Values to return. Two generic cases: the return
	// value is a set of Atoms, or a set of non-Atom Values.
	if (have_vals or body->is_type(LINK_SIGNATURE_LINK))
	{
		// Type kind = LinkSignatureLinkCast(body)->get_kind();
		// if (LINK_VALUE == kind) ...
		return createLinkValue(std::move(rew));
	}

	// A list of Handles.
	HandleSeq hseq;
	for (const ValuePtr& v : rew) hseq.emplace_back(HandleCast(v));
	return scratch->add_link(LIST_LINK, std::move(hseq));
}

ValuePtr FilterLink::do_execute(AtomSpace* as, bool silent) const
{
	ValuePtr vex(_outgoing[1]);

	if (_outgoing[1]->is_executable())
	{
		vex = _outgoing[1]->execute(as, silent);

		// If it's a container, and its not closed, then pull
		// one value out, and process it. Else if its closed,
		// fall through, and let the next stage handle it.
		if (vex->is_type(CONTAINER_VALUE))
		{
			ContainerValuePtr cvp = ContainerValueCast(vex);
			if (not cvp->is_closed())
			{
				ValuePtr mone = rewrite_one(cvp->remove(), as, silent);
				return createLinkValue(mone);
			}
			// If it is closed, fall through.
		}

		if (vex->is_type(LINK_VALUE))
		{
			ValueSeq remap;
			for (const ValuePtr& vp : LinkValueCast(vex)->value())
			{
				ValuePtr mone = rewrite_one(vp, as, silent);
				if (nullptr != mone) remap.emplace_back(mone);
			}
			return createLinkValue(std::move(remap));
		}
	}

	// Handle four different cases.
	// If there is a single Atom, apply the filter to the single Atom.
	// If there is a Set of Atoms, apply the filter to the set members.
	// If there is a List of Atoms, apply the filter to the list elts.
	// If there is a LinkValue, apply the filter to its elts.
	Type argtype = vex->get_type();
	if (SET_LINK == argtype or LIST_LINK == argtype)
	{
		Handle valh(HandleCast(vex));
		HandleSeq remap;
		for (const Handle& h : valh->getOutgoingSet())
		{
			Handle mone = HandleCast(rewrite_one(h, as, silent));
			if (nullptr != mone) remap.emplace_back(mone);
		}
		return as->add_link(argtype, std::move(remap));
	}

	// If we're getting a shim, we MUST unwrap it. If the shim
	// wraps a LinkValue, iterate over that.
	if (VALUE_SHIM_LINK == argtype)
	{
		Handle valh(HandleCast(vex));
		ValuePtr svp(valh->execute(as, silent));
		if (LINK_VALUE != svp->get_type()) return svp;

		ValueSeq remap;
		const ValueSeq& vsq(LinkValueCast(svp)->value());
		for (const ValuePtr& v: vsq)
			remap.emplace_back(rewrite_one(v, as, silent));

		if (1 == remap.size()) return remap[0];
		return createLinkValue(std::move(remap));
	}

	// Its a singleton. Just remap that.
	return rewrite_one(vex, as, silent);
}

ValuePtr FilterLink::execute(AtomSpace* as, bool silent)
{
	// This can execute recursively, if the AtomSpace is searched for
	// FilterLinks, and then the results passed through a FilterLink.
	// This is "rare", but the other outcome is stack exhaustion and
	// crash.
	if (_recursive_exec)
		return get_handle();

	_recursive_exec = true;
	ValuePtr vp(do_execute(as, silent));
	_recursive_exec = false;
	return vp;
}

DEFINE_LINK_FACTORY(FilterLink, FILTER_LINK)

/* ===================== END OF FILE ===================== */
