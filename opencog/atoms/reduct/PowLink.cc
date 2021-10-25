/*
 * opencog/atoms/reduct/PowLink.cc
 *
 * Copyright (C) 2021 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <math.h>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "ArithmeticLink.h"
#include "PowLink.h"

using namespace opencog;

PowLink::PowLink(const HandleSeq&& oset, Type t)
    : NumericFunctionLink(std::move(oset), t)
{
	init();
}

PowLink::PowLink(const Handle& a, const Handle& b)
    : NumericFunctionLink({a, b}, POW_LINK)
{
	init();
}

void PowLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, POW_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PowLink");

	size_t nargs = _outgoing.size();
	if (2 != nargs)
		throw InvalidParamException(TRACE_INFO,
			"PowLink expects one, got %s",
			to_string().c_str());
}

// ============================================================

ValuePtr PowLink::execute(AtomSpace* as, bool silent)
{
	ValueSeq reduction;
	ValuePtr result(apply_func(as, silent, _outgoing,
		pow, reduction));

	if (result) return result;

	// No numeric values available. Sorry!
	// Return the best-possible reduction that we did get.
	if (reduction[0]->is_atom() and reduction[1]->is_atom())
		return createPowLink(HandleCast(reduction[0]), HandleCast(reduction[1]));

	// Unable to reduce at all. Just return the original atom.
	return get_handle();
}

DEFINE_LINK_FACTORY(PowLink, POW_LINK);

// ============================================================
