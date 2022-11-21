/*
 * PromiseLink.cc
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
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/value/FormulaStream.h>
#include "PromiseLink.h"

using namespace opencog;

PromiseLink::PromiseLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, PROMISE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an PromiseLink, got %s", tname.c_str());
	}
	init();
}

PromiseLink::PromiseLink(const Handle& valb)
	: Link(std::move({valb}), PROMISE_LINK)
{
	init();
}

PromiseLink::PromiseLink(const Handle& valb, const Handle& typ)
	: Link(std::move({valb, typ}), PROMISE_LINK)
{
	init();
}

void PromiseLink::init(void)
{
	// The default future.
	_type = STREAM_VALUE;

	if (1 == _outgoing.size()) return;

	// The second Atom is a type specifying the kind of future
	// we should use.
	TypeNodePtr tnp = TypeNodeCast(_outgoing[1]);
	if (nullptr == tnp)
		throw SyntaxException(TRACE_INFO,
			"Expecting a TypeNode to specify the future type!");

	_type = tnp->get_kind();

	if (not ((STREAM_VALUE == _type) or (LINK_STREAM_VALUE == _type)))
		throw SyntaxException(TRACE_INFO,
			"Expecting a Stream of some kind!");
}

// ---------------------------------------------------------------

/// When executed, this will return the value at the indicated key.
ValuePtr PromiseLink::execute(AtomSpace* as, bool silent)
{
	if (silent)
		throw SilentException();

	throw InvalidParamException(TRACE_INFO,
	   "No value at key %s on atom %s",
	   ak->to_string().c_str(), ah->to_string().c_str());
}

DEFINE_LINK_FACTORY(PromiseLink, PROMISE_LINK)

/* ===================== END OF FILE ===================== */
