/*
 * DualLink.cc
 *
 * Copyright (C) 2014-2016 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/FreeLink.h>

#include "DualLink.h"

using namespace opencog;

void DualLink::init(void)
{
	_pat.redex_name = "anonymous DualLink";
}

DualLink::DualLink(const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(DUAL_LINK, hseq, tv, av)
{
	init();
}

DualLink::DualLink(Type t, const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(t, hseq, tv, av)
{
	init();
}

DualLink::DualLink(Link &l)
	: PatternLink(l)
{
	Type t = l.getType();
	if (not classserver().isA(t, DUAL_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a DualLink, got %s", tname.c_str());
	}

	init();
}

/* ===================== END OF FILE ===================== */
