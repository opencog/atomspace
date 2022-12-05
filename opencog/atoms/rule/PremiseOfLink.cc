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
#include <opencog/atoms/core/NumberNode.h>
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
	size_t sz = _outgoing.size();
	if (1 != sz and 2 != sz)
		throw SyntaxException(TRACE_INFO, "Expecting one or two arguments!");

	const Handle& ho = _outgoing[0];
	if (not ho->is_type(RULE_LINK))
		throw SyntaxException(TRACE_INFO, "Expecting a RuleLink!");

	const RuleLinkPtr& rule = RuleLinkCast(ho);
	if (1 == sz)
	{
		_premise = _body;
		return;
	}

	// TBD call NumericFunctionLink::get_value() ...
	const Handle& nu = _outgoing[1];
	if (not nu->is_type(NUMBER_NODE))
		throw SyntaxException(TRACE_INFO, "Expecting a NumberNode!");

	double off = NumberNodeCast(nu)->get_value();
	if (0.0 > off)
		throw InvalidParameterException(TRACE_INFO,
			"Expecting a  non-negative index");

	size_t idx = std::round(off);

	size_t nump = _body->size();
	if (nump <= idx)
		throw InvalidParameterException(TRACE_INFO,
			"Index is out-of-range: %lu vs %lu", nump, idx);

	_premise = _body->getOutgoingAtom(idx);
}

// ---------------------------------------------------------------

ValuePtr PremiseOfLink::execute(AtomSpace* as, bool silent)
{
	return _premise;
}

DEFINE_LINK_FACTORY(PremiseOfLink, PREMISE_OF_LINK)

/* ===================== END OF FILE ===================== */
