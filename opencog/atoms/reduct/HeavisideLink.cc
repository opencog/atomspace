/*
 * opencog/atoms/reduct/HeavisideLink.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "ArithmeticLink.h"
#include "HeavisideLink.h"

using namespace opencog;

HeavisideLink::HeavisideLink(const HandleSeq&& oset, Type t)
    : FunctionLink(std::move(oset), t)
{
	init();
}

HeavisideLink::HeavisideLink(const Handle& a)
    : FunctionLink({a}, HEAVISIDE_LINK)
{
	init();
}

HeavisideLink::HeavisideLink(const Handle& a, const Handle& b)
    : FunctionLink({a, b}, HEAVISIDE_LINK)
{
	init();
}

void HeavisideLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, HEAVISIDE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a HeavisideLink");

	size_t nargs = _outgoing.size();
	if (1 != nargs)
		throw InvalidParamException(TRACE_INFO,
			"HeavisideLink expects one, got %s",
			to_string().c_str());
}

// ============================================================

static double impulse(double x) {return signbit(x); }

ValuePtr HeavisideLink::execute(AtomSpace* as, bool silent)
{
	ValuePtr reduction;
	ValuePtr result(ArithmeticLink::apply_func(as, silent,
		_outgoing[0], impulse, reduction));

	if (result) return result;

	// No numeric values available. Sorry!
	// Return the best-possible reduction that we did get.
	if (reduction->is_atom())
		return createHeavisideLink(HandleCast(reduction));

	// Unable to reduce at all. Just return the original atom.
	return get_handle();
}

DEFINE_LINK_FACTORY(HeavisideLink, HEAVISIDE_LINK);

// ============================================================
