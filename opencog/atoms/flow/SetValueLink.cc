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
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/LambdaLink.h>
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
	// Obtain the value that we will be setting.
	ValuePtr pap;
	if (3 == _outgoing.size())
	{
		if (_outgoing[2]->is_executable())
			pap = _outgoing[2]->execute(as, silent);
		else
			pap = _outgoing[2];
	}
	else
	{
		Handle put(_outgoing[2]);
		Type pt = put->get_type();
		if (DEFINED_PREDICATE_NODE == pt or DEFINED_SCHEMA_NODE == pt)
		{
			put = DefineLink::get_definition(put);
			pt = put->get_type();
		}

		// XXX TODO we should perform a type-check to make sure
		// variable declarations match the args.
		if (not nameserver().isA(pt, LAMBDA_LINK))
			throw SyntaxException(TRACE_INFO,
				"Expecting a LambdaLink, got %s",
				put->to_string().c_str());

		LambdaLinkPtr lamp(LambdaLinkCast(put));
		const Handle& args(_outgoing[3]);
		Handle reduct;
		if (LIST_LINK == args->get_type())
			reduct = lamp->beta_reduce(args->getOutgoingSet());
		else
			reduct = lamp->beta_reduce({args});

		if (reduct->is_executable())
			pap = reduct->execute(as, silent);
		else
			pap = reduct;
	}

	// We cannot set Values unless we are working with the unique
	// version of the atom that sits in the AtomSpace!
	Handle ah(as->get_atom(_outgoing[0]));
	Handle ak(as->get_atom(_outgoing[1]));
	if (ah and ak)
	{
		ah->setValue(ak, pap);
		return pap;
	}

	// Hmm. shouldn't this be SilentException?
	if (silent)
		throw SilentException();

	throw InvalidParamException(TRACE_INFO,
		"No atom %s or no key %s",
		_outgoing[0]->to_string().c_str(),
		_outgoing[1]->to_string().c_str());
}

DEFINE_LINK_FACTORY(SetValueLink, SET_VALUE_LINK)

/* ===================== END OF FILE ===================== */
