/*
 * ImplicationLink.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Nil Geisweiller
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

#include "ImplicationLink.h"

using namespace opencog;

void ImplicationLink::init(void)
{
	extract_variables(_outgoing);
}

ImplicationLink::ImplicationLink(const HandleSeq& hseq,
                                 TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(IMPLICATION_LINK, hseq, tv, av)
{
	init();
}

ImplicationLink::ImplicationLink(Type t, const HandleSeq& hseq,
                                     TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(t, hseq, tv, av)
{
	init();
}

ImplicationLink::ImplicationLink(Link &l)
	: ScopeLink(l)
{
	Type t = l.getType();
	if (not classserver().isA(t, IMPLICATION_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a ImplicationLink, got %s", tname.c_str());
	}

	init();
}

void ImplicationLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = _outgoing.size();

	if (sz < 2 or 3 < sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set of size 2 or 3, got %d", sz);

	// If there is an explicit variable declaration, then use that.
	// If there is no variable declaration, then assume that all of
	// the free variables in the link are scoped.
	ScopeLink::extract_variables(oset);

	if (2 == sz)
	{
		_implicand = oset[1];
		return;
	}

	// If we are here, then the first outgoing set member should be
	// a variable declaration.
	_implicand = oset[2];
}
