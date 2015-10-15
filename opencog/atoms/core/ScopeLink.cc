/*
 * ScopeLink.cc
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
#include <opencog/atoms/core/LambdaLink.h>

#include "ScopeLink.h"

using namespace opencog;

void ScopeLink::init(void)
{
	size_t sz = _outgoing.size();
	if (2 < sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of at most two, got %d", sz);

	extract_variables(_outgoing);
}

ScopeLink::ScopeLink(const HandleSeq& oset,
                     TruthValuePtr tv, AttentionValuePtr av)
	: Link(SCOPE_LINK, oset, tv, av)
{
	init();
}

ScopeLink::ScopeLink(const Handle& vars, const Handle& body,
                     TruthValuePtr tv, AttentionValuePtr av)
	: Link(SCOPE_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

ScopeLink::ScopeLink(Type t, const Handle& body,
                     TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, HandleSeq({body}), tv, av)
{
	// Derived classes have a different initialization sequence
	if (SCOPE_LINK != t) return;
	init();
}

ScopeLink::ScopeLink(Type t, const HandleSeq& oset,
                     TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, oset, tv, av)
{
	// Derived classes have a different initialization sequence
	if (SCOPE_LINK != t) return;
	init();
}

ScopeLink::ScopeLink(Link &l)
	: Link(l)
{
	// Type must be as expected.
	Type tscope = l.getType();
	if (not classserver().isA(tscope, SCOPE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a ScopeLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (SCOPE_LINK != tscope) return;
	init();
}

/* ================================================================= */
///
/// Find and unpack variable declarations, if any; otherwise, just
/// find all free variables.
///
void ScopeLink::extract_variables(const HandleSeq& oset)
{
	Type decls = oset.at(0)->getType();

	// If the first atom is not explicitly a variable declaration, then
	// there are no variable declarations. There are two cases that; can
	// apply here: either the body is a lmabda, in whcih casse, we copy
	// the variables from the lambda; else we extract all free variables.
	if (VARIABLE_LIST != decls and
	    VARIABLE_NODE != decls and
	    TYPED_VARIABLE_LINK != decls and
	    GLOB_NODE != decls)
	{
		_body = oset[0];

		if (classserver().isA(_body->getType(), LAMBDA_LINK))
		{
			LambdaLinkPtr lam(LambdaLinkCast(_body));
			if (nullptr == lam)
				lam = createLambdaLink(*LinkCast(_body));
			_varlist = lam->get_variables();
			_body = lam->get_body();
		}
		else
		{
			// Use the FreeLink class to find all the variables;
			// Use the VariableList class for build the Variables struct.
			FreeLink fl(oset[0]);
			VariableList vl(fl.get_vars());
			_varlist = vl.get_variables();
		}
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
void ScopeLink::init_scoped_variables(const Handle& hvar)
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
