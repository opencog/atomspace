/*
 * ValueOfLink.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
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
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/reduct/NumericFunctionLink.h>
#include "ValueOfLink.h"

using namespace opencog;

ValueOfLink::ValueOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, VALUE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an ValueOfLink, got %s", tname.c_str());
	}
	init();
}

void ValueOfLink::init(void)
{
	size_t ary = _outgoing.size();
	if (0 == ary)
		throw SyntaxException(TRACE_INFO, "Expecting one or more atoms!");

	if (1 == ary and
		 (nameserver().isA(_type, TRUTH_VALUE_OF_LINK) or
		  nameserver().isA(_type, STRENGTH_OF_LINK) or
		  nameserver().isA(_type, CONFIDENCE_OF_LINK) or
		  nameserver().isA(_type, COUNT_OF_LINK) or
		  nameserver().isA(_type, FLOAT_VALUE_OF_LINK)))
	{
		return;
	}

	// Argh. FETCH_VALUE_OF can have 4 args.
	//if (3 < ary)
	//	throw SyntaxException(TRACE_INFO, "Expecting two or three atoms!");
}

// ---------------------------------------------------------------

/// When executed, this will return the value at the indicated key.
/// The idx_of_idx is where we can find the optional index atom.
ValuePtr ValueOfLink::do_execute(AtomSpace* as, bool silent, int idx_of_idx)
{
	// We cannot know the Value of the Atom unless we are
	// working with the unique version that sits in the
	// AtomSpace! It can happen, during evaluation e.g. of
	// a PutLink, that we are given an Atom that is not in
	// any AtomSpace. In this case, `as` will be a scratch
	// space; we can add the Atom there, and things will
	// trickle out properly in the end.
	//
	// XXX TODO FIXME ... if either of these are executable, then
	// they need to be executed, first, right? Because that's the
	// usual intent. Else they'd be wrapped in a DontExecLink, right?
	// I'm confused.
	Handle ah(as->add_atom(_outgoing[0]));
	Handle ak(as->add_atom(_outgoing[1]));

	ValuePtr pap = ah->getValue(ak);
	if (pap)
	{
		// If there's no third reference, we are done.
		if (0 > idx_of_idx)
			return pap;

		double offset = 0.0;
		ValuePtr nvp(NumericFunctionLink::get_value(as, silent, _outgoing[idx_of_idx]));
		if (nvp->is_type(NUMBER_NODE))
			offset = NumberNodeCast(nvp)->value()[0];
		else if (nvp->is_type(FLOAT_VALUE))
			offset = FloatValueCast(nvp)->value()[0];

		size_t idx = (size_t) (round (offset));
		return pap->value_at_index(idx);
	}

#if 0
	// Hmm. If there's no value, it might be because it was deleted.
	// There are many reasons for that. So, instead of throwing, we're
	// going to return a nullptr instead, and assume that all upstream
	// users are smart enough to check for that. It think they are,
	// but you never know...
	if (silent)
		throw SilentException();

	throw InvalidParamException(TRACE_INFO,
	   "No value at key %s on atom %s",
	   ak->to_string().c_str(), ah->to_string().c_str());
#endif

	return nullptr;
}

/// When executed, this will return the value at the indicated key.
ValuePtr ValueOfLink::execute(AtomSpace* as, bool silent)
{
	// Very special case: If no key is given, *and* its a FloatValueOf,
	// *and* the atom is a NumberNode, then case the number to a Float.
	size_t ary = _outgoing.size();
	if (1 == ary)
	{
		if (FLOAT_VALUE_OF_LINK == _type and
		    NUMBER_NODE == _outgoing[0]->get_type())
		{
			return createFloatValue(NumberNodeCast(_outgoing[0])->value());
		}
		throw InvalidParamException(TRACE_INFO,
			"Expecting an Atom and a key");
	}

	if (2 == ary)
		return do_execute(as, silent, -1);

	// Assume ary == 3
	return do_execute(as, silent, 2);
}

DEFINE_LINK_FACTORY(ValueOfLink, VALUE_OF_LINK)

/* ===================== END OF FILE ===================== */
