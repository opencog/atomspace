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
	// The default future.
	_future_type = FORMULA_STREAM;

	if (0 < _outgoing.size() and not _outgoing[0]->is_executable())
		throw SyntaxException(TRACE_INFO,
			"Expecting an executable Atom!");

	if (1 == _outgoing.size()) return;

	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting an executable Atom and an optional TypeNode!");

	// The second Atom is a type specifying the kind of future
	// we should use.
	TypeNodePtr tnp = TypeNodeCast(_outgoing[1]);
	if (nullptr == tnp)
		throw SyntaxException(TRACE_INFO,
			"Expecting a TypeNode to specify the future type!");

	_future_type = tnp->get_kind();

	if (not ((FORMULA_STREAM == _future_type) or (FUTURE_STREAM == _future_type)))
		throw SyntaxException(TRACE_INFO,
			"Expecting a Stream of some kind!");
}

// ---------------------------------------------------------------

/// When executed, this will wrap the promise with a future.
ValuePtr PromiseLink::execute(AtomSpace* as, bool silent)
{
	if (FORMULA_STREAM == _future_type)
		return createFormulaStream(_outgoing[0]);

	// if (FUTURE_STREAM == _future_type)
		return createFutureStream(_outgoing[0]);
}

DEFINE_LINK_FACTORY(PromiseLink, PROMISE_LINK)

/* ===================== END OF FILE ===================== */
