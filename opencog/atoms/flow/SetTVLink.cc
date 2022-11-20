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

	size_t ary = _outgoing.size();
	if (2 != ary and 3 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting two or three atoms!");
}

// ---------------------------------------------------------------

/// When evaluated, this will evaluate the second argument to obtain
/// a TruthValue, and then set that TruthValue on the indicated Atom
/// (first argument). The computed TV is returned.
TruthValuePtr SetTVLink::evaluate(AtomSpace* as, bool silent)
{
	size_t ary = _outgoing.size();

	// Obtain the value that we will be setting.
	TruthValuePtr tv;

	// The expression to evaluate
	Handle evex;
	if (2 == ary)
		evex = _outgoing[1];
	else if (3 == ary)
		evex = createEvaluationLink(_outgoing[1], _outgoing[2]);

	// Now, evaluate it.
	if (evex->is_evaluatable())
		tv = evex->evaluate(as, silent);
	else if (nameserver().isA(evex->get_type(), EVALUATABLE_LINK))
		tv = EvaluationLink::do_evaluate(as, evex, silent);
	else if (evex->is_executable())
	{
		ValuePtr vp = evex->execute(as, silent);
		Type vpt = vp->get_type();
		if (nameserver().isA(vpt, TRUTH_VALUE))
			tv = TruthValueCast(vp);
		else
		if (nameserver().isA(vpt, FLOAT_VALUE))
		{
			if (2 == FloatValueCast(vp)->value().size())
				tv = createSimpleTruthValue(vp);
			else
				tv = createCountTruthValue(vp);
		}
		else
			throw RuntimeException(TRACE_INFO,
				"Expecting a FloatValue or TruthValue, got %s",
				vp->to_string().c_str());
	}
	else
		tv = evex->getTruthValue();

	// We cannot set TVs unless we are working with the unique
	// version of the atom that sits in the AtomSpace!
	Handle ah(as->get_atom(_outgoing[0]));
	if (ah)
	{
		ah->setTruthValue(tv);
		return tv;
	}

	// Hmm. shouldn't this be SilentException?
	if (silent)
		throw SilentException();

	throw InvalidParamException(TRACE_INFO,
		"No atom %s",
		_outgoing[0]->to_string().c_str());
}

DEFINE_LINK_FACTORY(SetTVLink, SET_TV_LINK)

/* ===================== END OF FILE ===================== */
