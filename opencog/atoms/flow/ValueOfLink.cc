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
#include <opencog/atoms/value/VoidValue.h>
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

	return;

#if 0
	// Nothing to do, here.
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
#endif
}

// ---------------------------------------------------------------

/// When executed, this will return the value at the indicated key.
ValuePtr ValueOfLink::do_execute(AtomSpace* as, bool silent)
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
	// they need to be executed, first, right? Yes, they do! We
	// can currently get away with not doing this for two reasons:
	// In all existing code, the first Atom is always an anchor,
	// and is thus never executable. The second Atom always ends
	// inside some FunctionLink, where it does eventually get
	// executed, as needed. So in the current apps, this all works
	// fine. In the future... well, I'm not fiddling with this today.
	// Worst case scenario, app can use a DontExecLink.
	Handle ah(as->add_atom(_outgoing[0]));
	Handle ak(as->add_atom(_outgoing[1]));

	ValuePtr pap = ah->getValue(ak);
	if (pap) return pap;

	// If we are here, then no Value was found. If there is a
	// third Atom, then it specifies a default to use instead.
	if (2 < _outgoing.size())
		return _outgoing[2];

	// Hmm. If there's no value, it might be because it was deleted,
	// or maybe it was never set. There are many reasons for that.
	// So, instead of throwing, we're going to return a VoidValue
	// instead. This is better than returning a nullptr, which has
	// a way of making upstream callers do a null pointer deref.
#if 0
	if (silent)
		throw SilentException();

	throw InvalidParamException(TRACE_INFO,
	   "No value at key %s on atom %s",
	   ak->to_string().c_str(), ah->to_string().c_str());
#endif

	return createVoidValue();
	// return nullptr;
}

/// When executed, this will return the value at the indicated key.
ValuePtr ValueOfLink::execute(AtomSpace* as, bool silent)
{
	// Very special case: If no key is given, *and* its a FloatValueOf,
	// *and* the atom is a NumberNode, then cast the number to a Float.
	size_t ary = _outgoing.size();
	if (1 == ary)
	{
		if (FLOAT_VALUE_OF_LINK == _type and
		    NUMBER_NODE == _outgoing[0]->get_type())
		{
			return createFloatValue(NumberNodeCast(_outgoing[0])->value());
		}

		// Well, we could also be wrapping a unary ValueShim.
		// I think this only happens when there's a bug somewhere
		// else, but ... well, its not totally preposterous, so
		// go ahead and handle it.
		if (VALUE_SHIM_LINK == _outgoing[0]->get_type())
		{
			return _outgoing[0]->execute(as, silent);
		}
		throw InvalidParamException(TRACE_INFO,
			"Expecting an Atom and a key; got %s", to_string().c_str());
	}

	return do_execute(as, silent);
}

DEFINE_LINK_FACTORY(ValueOfLink, VALUE_OF_LINK)

/* ===================== END OF FILE ===================== */
