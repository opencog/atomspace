/*
 * StringOfLink.cc
 *
 * Copyright (C) 2022, 2024 Linas Vepstas
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
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/value/StringValue.h>
#include "StringOfLink.h"

using namespace opencog;

StringOfLink::StringOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, STRING_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an StringOfLink, got %s", tname.c_str());
	}
	init();
}

StringOfLink::StringOfLink(const Handle& ht, const Handle& hv)
	: FunctionLink({ht, hv}, STRING_OF_LINK)
{
	init();
}

void StringOfLink::init(void)
{
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting exactly two arguments!");

	bool oktype =
		_outgoing[0]->is_type(TYPE_NODE) or
		_outgoing[0]->is_executable();
	if (not oktype)
		throw SyntaxException(TRACE_INFO,
			"Expecting the a TypeNode as the first argument!");

	oktype =
		_outgoing[1]->is_type(NODE) or
		_outgoing[1]->is_type(STRING_VALUE) or
		_outgoing[1]->is_executable();
	if (not oktype)
		throw SyntaxException(TRACE_INFO,
			"Expecting the second argument to be a Node or StringValue!");
}

// ---------------------------------------------------------------

/// When executed, execute the args, and attempt to convert to
/// the requested type
ValuePtr StringOfLink::execute(AtomSpace* as, bool silent)
{
	Type to_type = NOTYPE;

	if (_outgoing[0]->is_executable())
	{
		ValuePtr vp = _outgoing[0]->execute();
		if (not vp->is_type(TYPE_NODE))
			throw InvalidParamException(TRACE_INFO,
				"Expecting a TypeNode, got %s",
				vp->to_string().c_str());
		to_type = TypeNodeCast(HandleCast(vp))->get_kind();
	}
	else
	if (_outgoing[0]->is_type(TYPE_NODE))
		to_type = TypeNodeCast(_outgoing[0])->get_kind();

	if (_outgoing[1]->is_executable())
	{
		ValuePtr vp = _outgoing[1]->execute();
		if (vp->is_type(NODE))
			return createNode(to_type, HandleCast(vp)->get_name());
		if (vp->is_type(STRING_VALUE))
			return createNode(to_type,
				StringValueCast(vp)->value()[0]);
		else
			throw InvalidParamException(TRACE_INFO,
				"Expecting a Node, got %s",
				vp->to_string().c_str());
	}
	else

	if (_outgoing[1]->is_type(NODE))
		return createNode(to_type, _outgoing[1]->get_name());

	/* Not reached */
	throw RuntimeException(TRACE_INFO,
		"Can't happen but it did %s", to_string().c_str());
}

DEFINE_LINK_FACTORY(StringOfLink, STRING_OF_LINK)

/* ===================== END OF FILE ===================== */
