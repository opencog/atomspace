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
#include "PremiseOfLink.h"

using namespace opencog;

PremiseOfLink::PremiseOfLink(const HandleSeq&& oset, Type t)
	: VardeclOfLink(std::move(oset), t)
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
	size_t sz = _outgoing.size();
	if (1 != sz and 2 != sz)
		throw SyntaxException(TRACE_INFO, "Expecting one or two arguments!");

	if (1 == sz)
		_premise = _lambda->get_body();
	else
		_premise = term_at(_lambda->get_body()->getOutgoingSet());

	// Make a copy of the bound vars in the lambda.
	Variables vars = _lambda->get_variables();
	vars.trim(_premise);
	_vardecl = vars.get_vardecl();
}

// ---------------------------------------------------------------

ValuePtr PremiseOfLink::execute(AtomSpace* as, bool silent)
{
	return as->add_link(LAMBDA_LINK, {_vardecl, _premise});
}

DEFINE_LINK_FACTORY(PremiseOfLink, PREMISE_OF_LINK)

/* ===================== END OF FILE ===================== */
