/*
 * GetLink.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomutils/FindUtils.h>

#include "PatternUtils.h"
#include "GetLink.h"

using namespace opencog;

void GetLink::init(void)
{
	extract_variables(_outgoing);
	unbundle_clauses(_body);
	common_init();
	_pat.redex_name = "anonymous GetLink";
}

GetLink::GetLink(const HandleSeq& hseq,
                 TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(GET_LINK, hseq, tv, av)
{
	init();
}

GetLink::GetLink(const Handle& body,
                 TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(GET_LINK, HandleSeq({body}), tv, av)
{
	init();
}

GetLink::GetLink(const Handle& vars, const Handle& body,
                 TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(GET_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

GetLink::GetLink(Type t, const HandleSeq& hseq,
                 TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(t, hseq, tv, av)
{
	// BindLink has a different clause initialization sequence
	if (GET_LINK != t) return;
	init();
}

GetLink::GetLink(Link &l)
	: PatternLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, GET_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a GetLink, got %s", tname.c_str());
	}

	// BindLink has a different initialization sequence
	if (GET_LINK != tscope) return;
	init();
}

/* ===================== END OF FILE ===================== */
