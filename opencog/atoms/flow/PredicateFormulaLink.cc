/*
 * PredicateFormulaLink.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include "PredicateFormulaLink.h"

using namespace opencog;

/// Custom variable extraction routine.
///
/// So, this is like a ScopeLink, except there do not need to be
/// any explicit variable decls, and the variables are usually free,
/// not typed, and there are *two* bodies, each body returning one
/// component of the final truth value...
///
/// XXX FIXME - in the future, some user is going to want to include
/// variable declarations, and/or an explicit Lambda in the body, for
/// some reason that I cannot imagine.  The code below will then fail.
/// For now, ignore this possibility.
void PredicateFormulaLink::init(void)
{
	_variables.find_variables(_outgoing);
}

PredicateFormulaLink::PredicateFormulaLink(const HandleSeq&& oset, Type t)
	: ScopeLink(std::move(oset), t)
{
	if (not nameserver().isA(t, PREDICATE_FORMULA_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an PredicateFormulaLink, got %s", tname.c_str());
	}
	if (2 != _outgoing.size() and 3 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting two or three arguments, got %s", to_string().c_str());

	init();
}

// ---------------------------------------------------------------

/// Evaluate a formula defined by this atom.
/// This returns a SimpleTruthValue, if there are two arguments,
/// and a CountTruthVaue, if there are three.
TruthValuePtr PredicateFormulaLink::apply(AtomSpace* as,
                                          const HandleSeq& cargs,
                                          bool silent)
{
	// Collect up two or three floating point values.
	std::vector<double> nums;
	for (const Handle& h: getOutgoingSet())
	{
		// An ordinary number.
		if (NUMBER_NODE == h->get_type())
		{
			nums.push_back(NumberNodeCast(h)->get_value());
			continue;
		}

		// In case the user wanted to wrap everything in a
		// LambdaLink. I don't understand why this is needed,
		// but it seems to make some people feel better, so
		// we support it.
		Handle flh(h);
		if (LAMBDA_LINK == h->get_type())
		{
			// Set flh and fall through, where it is executed.
			flh = LambdaLinkCast(h)->beta_reduce(cargs);
		}

		// At this point, we expect a FunctionLink of some kind.
		if (not nameserver().isA(flh->get_type(), FUNCTION_LINK))
			throw SyntaxException(TRACE_INFO, "Expecting a FunctionLink");

		// If the FunctionLink has free variables in it,
		// reduce them with the provided arguments.
		FunctionLinkPtr flp(FunctionLinkCast(flh));
		const FreeVariables& fvars = flp->get_vars();
		if (not fvars.empty())
		{
			flh = fvars.substitute_nocheck(flh, cargs);
		}

		// Expecting a FunctionLink without variables.
		ValuePtr v(flh->execute(as, silent));
		Type vtype = v->get_type();
		if (vtype == NUMBER_NODE)
		{
			nums.push_back(NumberNodeCast(v)->get_value());
			continue;
		}

		if (nameserver().isA(vtype, FLOAT_VALUE))
		{
			FloatValuePtr fv(FloatValueCast(v));
			nums.push_back(fv->value()[0]);
			continue;
		}

		// If it is neither NumberNode nor a FloatValue...
		throw RuntimeException(TRACE_INFO,
			"Expecting a FunctionLink that returns NumberNode/FloatValue");
	}

	if (nums.size() == 2)
		return createSimpleTruthValue(nums);
	return createCountTruthValue(nums);
}

// ---------------------------------------------------------------

/// A shortened, argument-free version of apply()
TruthValuePtr PredicateFormulaLink::evaluate(AtomSpace* as, bool silent)
{
	std::vector<double> nums;
	for (const Handle& h: getOutgoingSet())
	{
		if (NUMBER_NODE == h->get_type())
		{
			nums.push_back(NumberNodeCast(h)->get_value());
			continue;
		}

		if (not  h->is_executable())
			throw SyntaxException(TRACE_INFO, "Expecting an executable Link");

		ValuePtr v(h->execute(as, silent));
		Type vtype = v->get_type();

		if (NUMBER_NODE == vtype)
		{
			nums.push_back(NumberNodeCast(v)->get_value());
			continue;
		}

		if (nameserver().isA(vtype, FLOAT_VALUE))
		{
			FloatValuePtr fv(FloatValueCast(v));
			nums.push_back(fv->value().at(0));
			continue;
		}

		throw RuntimeException(TRACE_INFO,
			"Expecting a FunctionLink that returns NumberNode/FloatValue");
	}
	if (nums.size() == 2)
		return createSimpleTruthValue(nums);
	return createCountTruthValue(nums);
}

// ---------------------------------------------------------------

DEFINE_LINK_FACTORY(PredicateFormulaLink, PREDICATE_FORMULA_LINK)

/* ===================== END OF FILE ===================== */
