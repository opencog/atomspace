/*
 * SizeOfLink.cc
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

#include "SizeOfLink.h"

using namespace opencog;

SizeOfLink::SizeOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, SIZE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SizeOfLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Return a FloatValue scalar.
ValuePtr SizeOfLink::execute(AtomSpace* as, bool silent)
{
	size_t ary = 0;
	for (const Handle& h : _outgoing)
	{
		if (not h->is_executable())
		{
			ary += h->size();
			continue;
		}

		const ValuePtr& pap(h->execute(as, silent));
		ary += pap->size();
	}

	return createFloatValue((double)ary);
}

DEFINE_LINK_FACTORY(SizeOfLink, SIZE_OF_LINK)

/* ===================== END OF FILE ===================== */
