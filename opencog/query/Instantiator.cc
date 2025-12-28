/*
 * Instantiator.cc
 *
 * Copyright (C) 2009, 2014, 2015, 2025 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/grant/DefineLink.h>
#include <opencog/atoms/scope/QuoteReduce.h>

#include "Instantiator.h"

using namespace opencog;

/// Perform beta-reduction on the expression `expr`, using the `vmap`
/// to fish out values for variables.  The map holds pairs: the first
/// member of the pair is the variable; the second is the value that
/// should be used as its replacement.  (Note that "variables" do not
/// have to actually be VariableNode's; they can be any atom.)
static Handle beta_reduce(const Handle& expr, const GroundingMap& vmap)
{
	if (vmap.empty()) return expr;

	// Format conversion. FreeVariables::substitute_nocheck() performs
	// beta-reduction correctly, so we just use that. But we have to
	// jam the map into the format it expects.
	HandleSeq vals;
	FreeVariables crud;
	unsigned int idx = 0;
	for (const auto& pr : vmap)
	{
		crud.varseq.push_back(pr.first);
		crud.index.insert({pr.first, idx});
		vals.push_back(pr.second);
		idx++;
	}
	return crud.substitute_nocheck(expr, vals);
}

/**
 * instantiate -- create a grounded expression from an ungrounded one,
 * and then execute it.  That is, beta-reduce, then execute.
 *
 * Given a handle to an ungrounded expression, and a set of groundings,
 * this will create a grounded expression.
 *
 * The set of groundings is to be passed in with the map 'vars', which
 * maps variable names to their groundings -- it maps variable names to
 * atoms that already exist in the atomspace.  This method will then go
 * through all of the variables in the expression, and substitute them
 * with their grounding, creating a new expression. The new expression
 * is then executed in the provided AtomSpace.
 */
ValuePtr opencog::instantiate(AtomSpace* as,
                              const GroundingMap& varmap,
                              const Handle& expr,
                              bool silent)
{
	// throw, not assert, because this is a user error ...
	if (nullptr == expr)
		throw InvalidParamException(TRACE_INFO,
			"Asked to ground a null expression");

	Type t = expr->get_type();

	// Execute any DefinedPredicateNodes
	if (nameserver().isA(t, DEFINED_PREDICATE_NODE) or
	    nameserver().isA(t, DEFINED_SCHEMA_NODE))
	{
		Handle defn(DefineLink::get_definition(expr));
		if (not defn->is_executable())
			return defn;
		return defn->execute(as, silent);
	}

#if 1
	// Needed for AbsentUTest, DotLambdaTest, DotMashupTest.
	if (PUT_LINK == t)
	{
		// There are vars to be beta-reduced. Reduce them.
		ValuePtr reduced(beta_reduce(expr, varmap));

		// (PutLink (DeleteLink ...)) returns nullptr
		if (nullptr == reduced) return nullptr;

		// Nothing more to do, if not an atom.
		if (not reduced->is_atom()) return reduced;

		Handle grounded(HandleCast(reduced));
		return grounded->execute(as, silent);
	}
#endif

	// Beta-reduce, respecting quotes
	QuoteReduce qreduce(varmap);
	Handle grounded(qreduce.walk_tree(expr));

	// Fire executable links.
	// We currently exclude EVALUATABLE_LINK here; the evaluatables
	// will have (true == grounded->is_executable()) but currently
	// a dozen unit tests fail if we execute them. I don't know why.
	Type gt = grounded->get_type();
	if (nameserver().isA(gt, EXECUTABLE_LINK))
		return grounded->execute(as, silent);

	return grounded;
}

/* ===================== END OF FILE ===================== */
