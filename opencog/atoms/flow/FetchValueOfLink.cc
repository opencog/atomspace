/*
 * FetchValueOfLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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

#include <opencog/atoms/value/FloatValue.h>

#include "FetchValueOfLink.h"

using namespace opencog;

FetchValueOfLink::FetchValueOfLink(const HandleSeq&& oset, Type t)
	: ValueOfLink(std::move(oset), t)
{
	if (not nameserver().isA(t, FETCH_VALUE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an FetchValueOfLink, got %s", tname.c_str());
	}
	init();
}

void FetchValueOfLink::void(init)
{
}

// ---------------------------------------------------------------

/// Return a FloatValue scalar.
ValuePtr FetchValueOfLink::execute(AtomSpace* as, bool silent)
{
	return ValueOfLink::execute(as, silent);
}

DEFINE_LINK_FACTORY(FetchValueOfLink, FETCH_VALUE_OF_LINK)

/* ===================== END OF FILE ===================== */
