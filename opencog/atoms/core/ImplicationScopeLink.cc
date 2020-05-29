/*
 * ImplicationScopeLink.cc
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

#include "ImplicationScopeLink.h"

using namespace opencog;

void ImplicationScopeLink::init(void)
{
	extract_variables(_outgoing);
}

ImplicationScopeLink::ImplicationScopeLink(const HandleSeq&& hseq, Type t)
	: ScopeLink(std::move(hseq), t)
{
	init();
}

void ImplicationScopeLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = _outgoing.size();

	if (sz < 2 or 3 < sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set of size 2 or 3, got %d", sz);

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

DEFINE_LINK_FACTORY(ImplicationScopeLink, IMPLICATION_SCOPE_LINK)
