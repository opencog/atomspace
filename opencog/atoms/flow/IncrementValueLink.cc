/*
 * IncrementValueLink.cc
 *
 * Copyright (C) 2015, 2018, 2020 Linas Vepstas
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
#include <opencog/atoms/execution/ExecutionOutputLink.h>
#include <opencog/atoms/value/FormulaStream.h>
#include <opencog/atoms/value/FutureStream.h>
#include "IncrementValueLink.h"

using namespace opencog;

IncrementValueLink::IncrementValueLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, INCREMENT_VALUE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an IncrementValueLink, got %s", tname.c_str());
	}

	size_t ary = _outgoing.size();
	if (INCREMENT_VALUE_LINK == t and 3 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting three atoms!");
}

// ---------------------------------------------------------------

/// When executed, this will execute the third argument to obtain
/// a Value, and then set that Value at the indicated key on the
/// first argument. The computed value is returned.
ValuePtr IncrementValueLink::execute(AtomSpace* as, bool silent)
{
	// Avoid null-pointer deref due to user error.
	// This can happen with improperly built FilterLinks.
	// See commentary in ValueOfLink::do_execute() about why
	// we want an AtomSpace.
	if (nullptr == as)
		throw RuntimeException(TRACE_INFO,
			"Expecting AtomSpace, got null pointer for %s\n",
			to_string().c_str());

	Handle ah(as->add_atom(_outgoing[0]));
	Handle ak(as->add_atom(_outgoing[1]));

	ValuePtr pap;
	if (_outgoing[2]->is_executable())
		pap = _outgoing[2]->execute(as, silent);
	else
		pap = _outgoing[2];

	std::vector<double> dvec;

	return as->increment_count(ah, ak, dvec);
}

DEFINE_LINK_FACTORY(IncrementValueLink, INCREMENT_VALUE_LINK)

/* ===================== END OF FILE ===================== */
