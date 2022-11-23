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
	if (2 != ary)
	{
		if (nameserver().isA(_type, TRUTH_VALUE_OF_LINK) or
		    nameserver().isA(_type, STRENGTH_OF_LINK) or
		    nameserver().isA(_type, CONFIDENCE_OF_LINK) or
		    nameserver().isA(_type, COUNT_OF_LINK))
		{
			if (1 == ary) return;
			throw SyntaxException(TRACE_INFO, "Expecting one atom!");
		}

		throw SyntaxException(TRACE_INFO, "Expecting two atoms!");
	}
}

// ---------------------------------------------------------------

/// When executed, this will return the value at the indicated key.
ValuePtr ValueOfLink::execute(AtomSpace* as, bool silent)
{
	// We cannot know the Value of the Atom unless we are
	// working with the unique version that sits in the
	// AtomSpace! It can happen, during evaluation e.g. of
	// a PutLink, that we are given an Atom that is not in
	// any AtomSpace. In this case, `as` will be a scratch
	// space; we can add the Atom there, and things will
	// trickle out properly in the end.
	//
	Handle ah(as->add_atom(_outgoing[0]));
	Handle ak(as->add_atom(_outgoing[1]));

	ValuePtr pap = ah->getValue(ak);
	if (pap) return pap;

	if (silent)
		throw SilentException();

	throw InvalidParamException(TRACE_INFO,
	   "No value at key %s on atom %s",
	   ak->to_string().c_str(), ah->to_string().c_str());
}

DEFINE_LINK_FACTORY(ValueOfLink, VALUE_OF_LINK)

/* ===================== END OF FILE ===================== */
