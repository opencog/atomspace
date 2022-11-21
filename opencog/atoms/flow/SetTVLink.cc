/*
 * SetTVLink.cc
 *
 * Copyright (C) 2015, 2018, 2020 Linas Vepstas
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/PromiseTruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include "SetTVLink.h"

using namespace opencog;

SetTVLink::SetTVLink(const HandleSeq&& oset, Type t)
	: SetValueLink(std::move(oset), t)
{
	if (not nameserver().isA(t, SET_TV_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SetTVLink, got %s", tname.c_str());
	}

	if (_outgoing.size() < 2)
		throw SyntaxException(TRACE_INFO, "Expecting at least two atoms!");
}

// ---------------------------------------------------------------

/// Direct evaluation. The SetTV has only two args: the Atom to set,
/// and the Atom to get the TV from. If the second arg is evaluatable,
/// then evaluate it to get a TV, and slap that TV onto the first arg.
/// The TV thus obtained is returned.
TruthValuePtr SetTVLink::eval_direct(AtomSpace* as, bool silent)
{
	// Try to evaluate the second argument.
	const Handle& evex = _outgoing[1];
	if (evex->is_evaluatable())
		return evex->evaluate(as, silent);

	if (nameserver().isA(evex->get_type(), EVALUATABLE_LINK))
		return EvaluationLink::do_evaluate(as, evex, silent);

	// Maybe it's executable, and returns something that can
	// be converted to a TV.
	if (evex->is_executable())
	{
		ValuePtr vp = evex->execute(as, silent);
		Type vpt = vp->get_type();
		if (nameserver().isA(vpt, TRUTH_VALUE))
			return TruthValueCast(vp);

		if (nameserver().isA(vpt, FLOAT_VALUE))
		{
			if (2 == FloatValueCast(vp)->value().size())
				return createSimpleTruthValue(vp);

			return createCountTruthValue(vp);
		}

		throw RuntimeException(TRACE_INFO,
			"Expecting a FloatValue or TruthValue, got %s",
			vp->to_string().c_str());
	}

	// None of the above. It is a constant.
	return evex->getTruthValue();
}

/// Multiple arguments. Wrap them all up into an EvaluationLink,
/// and wrap that into a PromiseTruthValue.
TruthValuePtr SetTVLink::make_formula(AtomSpace* as, bool silent)
{
	HandleSeq args = _outgoing;
	args.erase(args.begin()); // drop the target

	Handle evl(createEvaluationLink(std::move(args)));
	evl = as->add_atom(evl);
	return createPromiseTruthValue(evl);
}

TruthValuePtr SetTVLink::evaluate(AtomSpace* as, bool silent)
{
	TruthValuePtr tv;
	if (2 == _outgoing.size())
		tv = eval_direct(as, silent);
	else
		tv = make_formula(as, silent);

	as->set_truthvalue(_outgoing[0], tv);
	return tv;
}

DEFINE_LINK_FACTORY(SetTVLink, SET_TV_LINK)

/* ===================== END OF FILE ===================== */
