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
 */

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/execution/ExecutionOutputLink.h>
#include <opencog/atoms/value/FormulaStream.h>
#include <opencog/atoms/value/FutureStream.h>
#include <opencog/atoms/value/VoidValue.h>
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
	if (2 > ary or 4 < ary)
		throw SyntaxException(TRACE_INFO,
			"Expecting two, three or four atoms!");
}

// ---------------------------------------------------------------

/// When executed, this will execute the third argument to obtain
/// a Value, and then set that Value at the indicated key on the
/// first argument. The computed value is returned.
/// The SetValueOn link returns the Atom, not the Value.
ValuePtr SetValueLink::execute(AtomSpace* as, bool silent)
{
	// Default VoidValue
	if (2 == _outgoing.size())
	{
		ValuePtr pap(createVoidValue());
		as->set_value(_outgoing[0], _outgoing[1], pap);
		if (SET_VALUE_ON_LINK == get_type())
			return _outgoing[0];
		return pap;
	}

	// Simple case: just set the Value. Obtain it, as needed.
	if (3 == _outgoing.size())
	{
		ValuePtr pap;
		if (_outgoing[2]->is_executable())
			pap = _outgoing[2]->execute(as, silent);
		else
			pap = _outgoing[2];

		as->set_value(_outgoing[0], _outgoing[1], pap);

		if (SET_VALUE_ON_LINK == get_type())
			return _outgoing[0];
		return pap;
	}

	// XXX Fixme Is this used anywhere ???
	// Its a vaguely spiffy idea, and it is tested in a unit test,
	// but is it actually used in any realistic scenario?
	// It would be coolear to just auto-wrap four or more args in
	// a LinkValue ... then again, this is easy for the user to do,
	// so not that big a deal, either way, I guess ...

	// Complicated Case: There are four arguments. The first two are
	// Atom and key, as before. Then comes a lambda or function in
	// third place, and the arguments to the function in fourth place.
	// Wrap these two in an ExecutionOutput, and then wrap that in a
	// FormulaStream. The user could do this themselves; this is
	// provided as a convenience function.

	const Handle& args(_outgoing[3]);
	Handle exo(createExecutionOutputLink(_outgoing[2], args));
	exo = as->add_atom(exo);

	ValuePtr fsp;
	if (args->is_type(NUMERIC_OUTPUT_SIG))
		fsp = createFormulaStream(exo);
	else
		fsp = createFutureStream(exo);

	as->set_value(_outgoing[0], _outgoing[1], fsp);

	if (SET_VALUE_ON_LINK == get_type())
		return _outgoing[0];
	return fsp;
}

DEFINE_LINK_FACTORY(SetValueLink, SET_VALUE_LINK)

/* ===================== END OF FILE ===================== */
