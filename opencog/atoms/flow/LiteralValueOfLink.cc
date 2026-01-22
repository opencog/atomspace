/*
 * LiteralValueOfLink.cc
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
#include "LiteralValueOfLink.h"

using namespace opencog;

LiteralValueOfLink::LiteralValueOfLink(const HandleSeq&& oset, Type t)
	: ValueOfLink(std::move(oset), t)
{
	if (not nameserver().isA(t, LITERAL_VALUE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an LiteralValueOfLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// When executed, this will return the value at the indicated key.
ValuePtr LiteralValueOfLink::execute(AtomSpace* as, bool silent)
{
	ValuePtr pap(get_literal(as, silent));
	if (pap) return pap;

	// Oh no! Nothing was found!
	if (silent)
		throw SilentException();

	throw InvalidParamException(TRACE_INFO,
	   "No value at key %s on atom %s",
	   ak->to_string().c_str(), ah->to_string().c_str());

	return createVoidValue();
}

DEFINE_LINK_FACTORY(LiteralValueOfLink, LITERAL_VALUE_OF_LINK)

/* ===================== END OF FILE ===================== */
