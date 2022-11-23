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
#include <opencog/atoms/value/FutureStream.h>
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
	: Link({valb}, PROMISE_LINK)
{
	init();
}

PromiseLink::PromiseLink(const Handle& valb, const Handle& typ)
	: Link({valb, typ}, PROMISE_LINK)
{
	init();
}

void PromiseLink::init(void)
{
	if (0 == _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting at least one executable Atom!");

	_future_type = NOTYPE;

	// Hunt down a TypeNode, if there is one
	for (const Handle& h : _outgoing)
	{
		if (TYPE_NODE == h->get_type())
		{
			TypeNodePtr tnp = TypeNodeCast(h);
			if (NOTYPE == _future_type)
				_future_type = tnp->get_kind();
			else
				throw SyntaxException(TRACE_INFO,
					"Expecting at most one Type specification");
			continue;
		}
		_args.push_back(h);
	}

	if (NOTYPE == _future_type)
		_future_type = FORMULA_STREAM;

	if (not ((FORMULA_STREAM == _future_type) or (FUTURE_STREAM == _future_type)))
		throw SyntaxException(TRACE_INFO,
			"Expecting a Stream of some kind!");

	// Unwrap a ListLink, if that's what we got.
	if (1 == _args.size() and LIST_LINK == _args[0]->get_type())
		_args = _args[0]->getOutgoingSet();

	for (const Handle& h : _args)
		if (not h->is_executable())
			throw SyntaxException(TRACE_INFO,
				"Expecting an executable Atom, got %s", h->to_string().c_str());
}

// ---------------------------------------------------------------

/// When executed, this will wrap the promise with a future.
ValuePtr PromiseLink::execute(AtomSpace* as, bool silent)
{
	HandleSeq oset = _args;

	if (FORMULA_STREAM == _future_type)
		return createFormulaStream(std::move(oset));

	return createFutureStream(std::move(oset));
}

DEFINE_LINK_FACTORY(PromiseLink, PROMISE_LINK)

/* ===================== END OF FILE ===================== */
