/*
 * PremiseOfLink.cc
 *
 * Copyright (C) 2015, 2018, 2022 Linas Vepstas
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
#include "RuleLink.h"
#include "PremiseOfLink.h"

using namespace opencog;

PremiseOfLink::PremiseOfLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, PREMISE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an PremiseOfLink, got %s", tname.c_str());
	}
	init();
}

void PremiseOfLink::init(void)
{
	if (1 != _outgoing.size())
		throw SyntaxException(TRACE_INFO, "Expecting only one argument!");

	const Handle& ho = _outgoing[0];
	if (not ho->is_type(RULE_LINK))
		throw SyntaxException(TRACE_INFO, "Expecting a RuleLink!");

	const RuleLinkPtr& rule = RuleLinkCast(ho);

}

// ---------------------------------------------------------------

/// When executed, this will return the value at the indicated key.
ValuePtr PremiseOfLink::execute(AtomSpace* as, bool silent)
{
	return _premise;
}

DEFINE_LINK_FACTORY(PremiseOfLink, PREMISE_OF_LINK)

/* ===================== END OF FILE ===================== */
