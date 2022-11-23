/*
 * NumberOfLink.cc
 *
 * Copyright (C) 2022 Linas Vepstas
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
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include "NumberOfLink.h"

using namespace opencog;

NumberOfLink::NumberOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, NUMBER_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an NumberOfLink, got %s", tname.c_str());
	}
	init();
}

NumberOfLink::NumberOfLink(const Handle& h)
	: FunctionLink({h}, NUMBER_OF_LINK)
{
	init();
}

void NumberOfLink::init(void)
{
	if (1 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting exactly one argument!");

	if (not _outgoing[0]->is_executable())
		throw SyntaxException(TRACE_INFO,
			"Expecting the argument to be executable!");
}

// ---------------------------------------------------------------

/// When executed, execute the arg, and attempt to convert it
/// to a NumberNode.
ValuePtr NumberOfLink::execute(AtomSpace* as, bool silent)
{
	ValuePtr pap = _outgoing[0]->execute(as, silent);

	// No conversion needed!
	if (pap->is_type(NUMBER_NODE))
		return pap;

	// Can we convert?
	FloatValuePtr fvp = FloatValueCast(pap);
	if (nullptr == fvp)
	{
		if (silent)
			throw SilentException();

		throw InvalidParamException(TRACE_INFO,
	   	"Expecting a FloatValue, got %s",
	   	pap->to_string().c_str());
	}

	return createNumberNode(fvp->value());
}

DEFINE_LINK_FACTORY(NumberOfLink, NUMBER_OF_LINK)

/* ===================== END OF FILE ===================== */
