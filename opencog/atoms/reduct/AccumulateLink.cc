/*
 * opencog/atoms/reduct/AccumulateLink.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/value/LinkValue.h>
#include "AccumulateLink.h"

using namespace opencog;

AccumulateLink::AccumulateLink(const HandleSeq&& oset, Type t)
    : NumericFunctionLink(std::move(oset), t)
{
	init();
}

AccumulateLink::AccumulateLink(const Handle& a)
    : NumericFunctionLink({a}, ACCUMULATE_LINK)
{
	init();
}

void AccumulateLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, ACCUMULATE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a AccumulateLink");

	size_t nargs = _outgoing.size();
	if (1 != nargs)
		throw InvalidParamException(TRACE_INFO,
			"AccumulateLink expects one, got %s",
			to_string().c_str());
}

// ============================================================

ValuePtr AccumulateLink::execute(AtomSpace* as, bool silent)
{
	// get_value() causes execution to happen on the arguments
	ValuePtr vi(get_value(as, silent, _outgoing[0]));
	Type vitype = vi->get_type();

	// If its a plain number, assume it's a vector, and sum.
	if (NUMBER_NODE == vitype)
	{
		const std::vector<double>& dvec(NumberNodeCast(vi)->value());
		double acc = 0.0;
		for (double dv : dvec)
			acc += dv;
		return createNumberNode(acc);
	}

	// If its a float value, it's a vector. Sum.
	if (nameserver().isA(vitype, FLOAT_VALUE))
	{
		const std::vector<double>& dvec(FloatValueCast(vi)->value());
		double acc = 0.0;
		for (double dv : dvec)
			acc += dv;
		return createFloatValue(acc);
	}

	// If it's a link value, assume its a list of floats. Sum.
	if (nameserver().isA(vitype, LINK_VALUE))
	{
		const std::vector<ValuePtr>& lvec(LinkValueCast(vi)->value());
		std::vector<double> acc;
		for (const ValuePtr& lv : lvec)
		{
			Type lvtype = lv->get_type();
			if (not nameserver().isA(lvtype, FLOAT_VALUE)) continue;

			const std::vector<double>& dvec(FloatValueCast(lv)->value());

			if (acc.size() < dvec.size())
				acc.resize(dvec.size());
			acc = plus(acc, dvec);
		}
		return createFloatValue(acc);
	}

	// If it did not fully reduce, then return the best-possible
	// reduction that we did get.
	if (vi->is_atom())
		return createAccumulateLink(HandleCast(vi));

	// Unable to reduce at all. Just return the original atom.
	return get_handle();
}

DEFINE_LINK_FACTORY(AccumulateLink, ACCUMULATE_LINK);

// ============================================================
