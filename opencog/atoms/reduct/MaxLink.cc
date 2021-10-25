/*
 * opencog/atoms/reduct/MaxLink.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <cfloat>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "MaxLink.h"

using namespace opencog;

MaxLink::MaxLink(const HandleSeq&& oset, Type t)
    : NumericOutLink(std::move(oset), t)
{
	init();
}

void MaxLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, MAX_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a MaxLink");
}

// ============================================================

ValuePtr MaxLink::execute(AtomSpace* as, bool silent)
{
	Type result_type = FLOAT_VALUE;
	std::vector<double> result;
	size_t len = SIZE_MAX;
	HandleSeq nan;

	for (const Handle& arg: _outgoing)
	{
		ValuePtr vi(get_value(as, silent, arg));
		Type vitype = vi->get_type();

		if (NUMBER_NODE == vitype)
		{
			result_type = NUMBER_NODE;
			const std::vector<double>& dvec(NumberNodeCast(vi)->value());
			len = std::min(len, dvec.size());
			result.resize(len, -DBL_MAX);
			for (size_t i = 0; i<len; i++)
				result[i] = std::max(result[i], dvec[i]);
		}
		else if (nameserver().isA(vitype, FLOAT_VALUE))
		{
			const std::vector<double>& dvec(FloatValueCast(vi)->value());
			len = std::min(len, dvec.size());
			result.resize(len, -DBL_MAX);
			for (size_t i = 0; i<len; i++)
				result[i] = std::max(result[i], dvec[i]);
		}
		else
			nan.push_back(arg);
	}

	// Unable to reduce at all. Just return the original atom.
	if (nan.size() == _outgoing.size())
		return get_handle();

	// If it did not fully reduce, then return the best-possible
	// reduction that we did get.
	if (0 < nan.size())
	{
		nan.push_back(HandleCast(createNumberNode(result)));
		return createMaxLink(std::move(nan));
	}

	if (FLOAT_VALUE == result_type)
		return createFloatValue(result);

	return createNumberNode(result);
}

DEFINE_LINK_FACTORY(MaxLink, MAX_LINK);

// ============================================================
