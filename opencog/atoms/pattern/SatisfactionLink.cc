/*
 * SatisfactionLink.cc
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

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/Satisfier.h>

#include "SatisfactionLink.h"

using namespace opencog;

void SatisfactionLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, SATISFACTION_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a SatisfactionLink, got %s", tname.c_str());
	}
}

SatisfactionLink::SatisfactionLink(const HandleSeq&& hseq, Type t)
	: PatternLink(std::move(hseq), t)
{
	init();
}

TruthValuePtr SatisfactionLink::evaluate(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;
	Satisfier sater(as);
	sater.satisfy(PatternLinkCast(get_handle()));

	// If there is an anchor, then attach results to the anchor.
	if (_variables._anchor and as)
	{
		as->add_link(MEMBER_LINK, sater._ground, _variables._anchor);
	}

	return sater._result;
}

DEFINE_LINK_FACTORY(SatisfactionLink, SATISFACTION_LINK)

/* ===================== END OF FILE ===================== */
