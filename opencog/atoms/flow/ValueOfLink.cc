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
}

// ---------------------------------------------------------------

/// When executed, this will return the value at the indicated key.
ValuePtr ValueOfLink::execute(AtomSpace* as, bool silent)
{
	size_t ary = _outgoing.size();
	if (2 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting two atoms!");

	// We cannot know the Value of the Atom unless we are
	// working with the unique version that sits in the AtomSpace!
	Handle ah(as->get_atom(_outgoing[0]));
	Handle ak(as->get_atom(_outgoing[1]));
	if (ah and ak)
	{
		ValuePtr pap = ah->getValue(ak);
		if (pap) return pap;

		if (silent)
			throw SilentException();

		throw InvalidParamException(TRACE_INFO,
		   "No value at key %s on atom %s",
		   ak->to_string().c_str(), ah->to_string().c_str());
	}

	if (silent)
		throw SilentException();

	// If the user asked for a Value not in any atomspace,
	// what should we do? I dunno, so I'm throwing an error.
	throw InvalidParamException(TRACE_INFO,
	   "Asked for a Value of atom not in any atomspace: %s",
	   this->to_string().c_str());
}

DEFINE_LINK_FACTORY(ValueOfLink, VALUE_OF_LINK)

/* ===================== END OF FILE ===================== */
