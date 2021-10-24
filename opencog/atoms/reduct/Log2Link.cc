/*
 * opencog/atoms/reduct/Log2Link.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "ArithmeticLink.h"
#include "Log2Link.h"

using namespace opencog;

Log2Link::Log2Link(const HandleSeq&& oset, Type t)
    : FunctionLink(std::move(oset), t)
{
	init();
}

Log2Link::Log2Link(const Handle& a)
    : FunctionLink({a}, LOG2_LINK)
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
	ValuePtr vi(ArithmeticLink::get_value(as, silent, _outgoing[0]));
	Type vitype = vi->get_type();

	if (NUMBER_NODE == vitype)
	{
		const std::vector<double>& dvec(NumberNodeCast(vi)->value());
		std::vector<double> gtvec;
		for (double dv : dvec)
		{
			gtvec.push_back(log2(dv));
		}
		return createNumberNode(gtvec);
	}

	if (nameserver().isA(vitype, FLOAT_VALUE))
	{
		const std::vector<double>& dvec(FloatValueCast(vi)->value());
		std::vector<double> gtvec;
		for (double dv : dvec)
		{
			gtvec.push_back(log2(dv));
		}
		return createFloatValue(gtvec);
	}

	// If it did not fully reduce, then return the best-possible
	// reduction that we did get.
	if (vi->is_atom())
		return createLog2Link(HandleCast(vi));

	// Unable to reduce at all. Just return the original atom.
	return get_handle();
}

DEFINE_LINK_FACTORY(Log2Link, LOG2_LINK);

// ============================================================
