/*
 * opencog/atoms/reduct/ArithmeticLink.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
 * All Rights Reserved
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

#include <limits>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include "ArithmeticLink.h"

using namespace opencog;

ArithmeticLink::ArithmeticLink(const HandleSeq&& oset, Type t)
    : FoldLink(std::move(oset), t)
{
	init();
}

void ArithmeticLink::init(void)
{
	Type tscope = get_type();
	if (ARITHMETIC_LINK == tscope)
		throw InvalidParamException(TRACE_INFO,
			"ArithmeticLinks are private and cannot be instantiated.");

	if (not nameserver().isA(tscope, ARITHMETIC_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an ArithmeticLink");

	_commutative = false;
}

// ===========================================================
/// delta_reduce() -- delta-reduce the expression by summing constants, etc.
///
/// Recall the definition of delta-reduction: it is the replacement of
/// functions with values by the value that the function would have.
/// For example, the delta-reduction of 2+2 is 4.
///
/// No actual black-box evaluation or execution is performed. Only
/// clearbox reductions are performed.
///
/// Examples: the delta-reduction of (FoldLink (Number 2) (Number 2))
/// is (Number 4) -- its just a constant.
///
/// The delta-reduct of (FoldLink (VariableNode "$x") (NumberNode 0))
/// is (VariableNode "$x"), because adding zero to anything yields the
/// thing itself.
///
/// This is certainly NOT a simple, easy-to-maintain way to build a
/// computer algebra system!  It works, its just barely good enough
/// for single-variable arithmetic, but that's all.  For general
/// reduction tasks, there are two choices:
///
/// A) Find some other library that does CAS, convert atoms to whatever
///    format that library uses, reduce that, and then convert back to
///    atoms.
///
/// B) Implement reduction with the Rule Engine, together with a set
///    of reduction rules for arithmetic.
///
/// In some sense B) is better, but is likely to have poorer performance
/// than A).  It also threatens to spiral out of control: We can add
/// ever-more rules to the rule engine to reduce ever-more interesting
/// algebraic expressions. CAS is actually hard.
///
ValuePtr ArithmeticLink::delta_reduce(AtomSpace* as, bool silent) const
{
	Handle road(reorder());
	ArithmeticLinkPtr alp(ArithmeticLinkCast(road));

	ValuePtr red(alp->FoldLink::delta_reduce(as, silent));

	if (nullptr == red or not red->is_atom()) return red;

	alp = ArithmeticLinkCast(HandleCast(red));
	if (nullptr == alp) return red;
	return alp->reorder();
}

// ============================================================

/// re-order the contents of an ArithmeticLink into "lexicographic" order.
/// This provides a canonical order that helps guarantee reduction.
///
/// The goal of the re-ordering is to simplify the reduction code,
/// by placing atoms where they are easily found.  For now, this
/// means:
/// first, all of the variables,
/// next, all compound expressions,
/// last, all number nodes
/// We do not currently sort the variables, but maybe we should...?
/// Sorting by variable names would hold consilidate them...
/// The FoldLink::delta_reduce() method already returns expressions that are
/// almost in the correct order.
Handle ArithmeticLink::reorder(void) const
{
	if (not _commutative) return get_handle();

	HandleSeq vars;
	HandleSeq exprs;
	HandleSeq numbers;

	for (const Handle& h : _outgoing)
	{
		Type htype = h->get_type();

		// Hack for pattern matcher, which returns SetLinks of stuff.
		// Recurse exactly once.
		if (SET_LINK == htype)
		{
			for (const Handle& he : h->getOutgoingSet())
			{
				Type het = he->get_type();
				if (VARIABLE_NODE == het)
					vars.push_back(he);
				else if (NUMBER_NODE == het)
					numbers.push_back(he);
				else
					exprs.push_back(he);
			}
		}
		else if (VARIABLE_NODE == htype)
			vars.push_back(h);
		else if (NUMBER_NODE == htype)
			numbers.push_back(h);
		else
			exprs.push_back(h);
	}

	HandleSeq result;
	for (const Handle& h : vars) result.push_back(h);
	for (const Handle& h : exprs) result.push_back(h);
	for (const Handle& h : numbers) result.push_back(h);

	return Handle(createLink(std::move(result), get_type()));
}

// ===========================================================

/// execute() -- Execute the expression
ValuePtr ArithmeticLink::execute(AtomSpace* as, bool silent)
{
	return delta_reduce(as, silent);
}

// ===========================================================
