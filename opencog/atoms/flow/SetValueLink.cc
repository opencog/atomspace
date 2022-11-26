/*
 * SetValueLink.cc
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
#include "SetValueLink.h"

using namespace opencog;

SetValueLink::SetValueLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, SET_VALUE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SetValueLink, got %s", tname.c_str());
	}

	size_t ary = _outgoing.size();
	if (SET_VALUE_LINK == t and 3 != ary and 4 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting three or four atoms!");
}

// ---------------------------------------------------------------

/// When executed, this will execute the third argument to obtain
/// a Value, and then set that Value at the indicated key on the
/// first argument. The computed value is returned.
ValuePtr SetValueLink::execute(AtomSpace* as, bool silent)
{
	// Simple case: just set the Value. Obtain it, as needed.
	if (3 == _outgoing.size())
	{
		ValuePtr pap;
		if (_outgoing[2]->is_executable())
			pap = _outgoing[2]->execute(as, silent);
		else
			pap = _outgoing[2];

		as->set_value(_outgoing[0], _outgoing[1], pap);
		return pap;
	}

	// Complicated Case: There are four arguments. The first two are
	// Atom and key, as before. Then comes a lambda or function in
	// third place, and the arguments to the function in fourth place.
	// Wrap these two in an ExecutionOutput, and then wrap that in a
	// FormulaStream. The user could do this themselves; this is
	// provided as a convenience function. Note that SetTV works the
	// same way.

	Handle exo(createExecutionOutputLink(_outgoing[2], _outgoing[3]));
	exo = as->add_atom(exo);
	ValuePtr fsp = createFormulaStream(exo);
	as->set_value(_outgoing[0], _outgoing[1], fsp);
	return fsp;
}

DEFINE_LINK_FACTORY(SetValueLink, SET_VALUE_LINK)

/* ===================== END OF FILE ===================== */
