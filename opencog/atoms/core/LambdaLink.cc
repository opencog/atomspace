/*
 * LambdaLink.cc
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
#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/core/FreeLink.h>

#include "LambdaLink.h"

using namespace opencog;

void LambdaLink::init(void)
{
	extract_variables(_outgoing);
}

LambdaLink::LambdaLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(LAMBDA_LINK, oset, tv, av)
{
	init();
}

LambdaLink::LambdaLink(const Handle& vars, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(LAMBDA_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

LambdaLink::LambdaLink(Type t, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, oset, tv, av)
{
	// Derived classes have a different initialization sequence
	if (LAMBDA_LINK != t) return;
	init();
}

LambdaLink::LambdaLink(Link &l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, LAMBDA_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a LambdaLink, got %s", tname.c_str());
	}

	// Dervided types have a different initialization sequence
	if (LAMBDA_LINK != tscope) return;
	init();
}

/* ================================================================= */
///
/// Find and unpack variable declarations, if any; otherwise, just
/// find all free variables.
///
/// On top of that initialize _body with the clauses of the
/// PatternLink.
///
void LambdaLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = oset.size();
	if (2 < sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of at most two, got %d", sz);

	// If the outgoing set size is one, then there are no variable
	// declarations; extract all free variables.
	if (1 == sz)
	{
		_body = oset[0];

		// Use the FreeLink class to find all the variables;
		// Use the VariableList class for build the Variables struct.
		FreeLink fl(oset[0]);
		VariableList vl(fl.get_vars());
		_varlist = vl.get_variables();
		return;
	}

	// If we are here, then the first outgoing set member should be
	// a variable declaration.
	_body = oset[1];

	// Initialize _varlist with the scoped variables
	init_scoped_variables(oset[0]);
}

/* ================================================================= */
///
/// Initialize _varlist given a handle of either VariableList or a
/// variable.
///
void LambdaLink::init_scoped_variables(const Handle& hvar)
{
	// Either it is a VariableList, or its a naked variable, or its a
	// typed variable.  Use the VariableList class as a tool to
	// extract the variables for us.
	Type t = hvar->getType();
	if (VARIABLE_LIST == t)
	{
		VariableList vl(LinkCast(hvar)->getOutgoingSet());
		_varlist = vl.get_variables();
	}
	else
	{
		VariableList vl({hvar});
		_varlist = vl.get_variables();
	}
}

/* ===================== END OF FILE ===================== */
