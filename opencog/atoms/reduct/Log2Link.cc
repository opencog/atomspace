/*
 * opencog/atoms/reduct/Log2Link.cc
 *
 * Copyright (C) 2021 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <math.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "Log2Link.h"

using namespace opencog;

Log2Link::Log2Link(const HandleSeq&& oset, Type t)
    : NumericFunctionLink(std::move(oset), t)
{
	init();
}

Log2Link::Log2Link(const Handle& a)
    : NumericFunctionLink({a}, LOG2_LINK)
{
	init();
}

void Log2Link::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, LOG2_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a Log2Link");

	size_t nargs = _outgoing.size();
	if (1 != nargs)
		throw InvalidParamException(TRACE_INFO,
			"Log2Link expects one, got %s",
			to_string().c_str());
}

// ============================================================

ValuePtr Log2Link::execute(AtomSpace* as, bool silent)
{
	ValuePtr reduction;
	ValuePtr result(apply_func(as, silent,
		_outgoing[0], log2, reduction));

	if (result) return result;

	// No numeric values available. Sorry!
	// Return the best-possible reduction that we did get.
	if (reduction->is_atom())
			return createLog2Link(HandleCast(reduction));

	// Unable to reduce at all. Just return the original atom.
	return get_handle();
}

DEFINE_LINK_FACTORY(Log2Link, LOG2_LINK);

// ============================================================
