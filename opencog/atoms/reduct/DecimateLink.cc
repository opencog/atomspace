/*
 * opencog/atoms/reduct/DecimateLink.cc
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/reduct/NumericFunctionLink.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include "DecimateLink.h"

using namespace opencog;

DecimateLink::DecimateLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t)
{
	init();
}

DecimateLink::DecimateLink(const Handle& a, const Handle& b)
    : Link({a, b}, DECIMATE_LINK)
{
	init();
}

void DecimateLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, DECIMATE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a DecimateLink");

	size_t nargs = _outgoing.size();
	if (2 != nargs)
		throw InvalidParamException(TRACE_INFO,
			"DecimateLink expects two arguments, got %s",
			to_string().c_str());
}

// ============================================================

ValuePtr DecimateLink::execute(AtomSpace* as, bool silent)
{
	// get_value() causes execution to happen on the arguments
	ValuePtr vm(NumericFunctionLink::get_value(as, silent, _outgoing[0]));
	BoolValuePtr bvp = BoolValueCast(vm);

	if (nullptr == bvp)
		throw SyntaxException(TRACE_INFO, "Mask must be a BoolValue!");

	const std::vector<bool>& vmask = bvp->value();

	ValuePtr vi(NumericFunctionLink::get_value(as, silent, _outgoing[1]));
	Type vitype = vi->get_type();

	// Get the shorter of the two.
	size_t len = std::min(vm->size(), vi->size());

	// Handle the various value types. Try the most likely ones first.
	// If its a float value, it's a vector.
	if (nameserver().isA(vitype, FLOAT_VALUE))
	{
		const std::vector<double>& dvec(FloatValueCast(vi)->value());
		std::vector<double> chopped;
		for (size_t i=0; i<len; i++)
			if (vmask[i]) chopped.push_back(dvec[i]);
		return createFloatValue(chopped);
	}

	// A vector of bools.
	if (nameserver().isA(vitype, BOOL_VALUE))
	{
		const std::vector<bool>& bvec(BoolValueCast(vi)->value());
		std::vector<bool> chopped;
		for (size_t i=0; i<len; i++)
			if (vmask[i]) chopped.push_back(bvec[i]);
		return createBoolValue(chopped);
	}

	// If its a plain number, assume it's a vector.
	if (NUMBER_NODE == vitype)
	{
		const std::vector<double>& dvec(NumberNodeCast(vi)->value());
		std::vector<double> chopped;
		for (size_t i=0; i<len; i++)
			if (vmask[i]) chopped.push_back(dvec[i]);
		return createNumberNode(chopped);
	}

	// A vector of other things.
	if (nameserver().isA(vitype, LINK_VALUE))
	{
		const std::vector<ValuePtr>& lvec(LinkValueCast(vi)->value());
		std::vector<ValuePtr> chopped;
		for (size_t i=0; i<len; i++)
			if (vmask[i]) chopped.push_back(lvec[i]);
		return createLinkValue(chopped);
	}

	// A vector of strings
	if (nameserver().isA(vitype, STRING_VALUE))
	{
		const std::vector<std::string>& svec(StringValueCast(vi)->value());
		std::vector<std::string> chopped;
		for (size_t i=0; i<len; i++)
			if (vmask[i]) chopped.push_back(svec[i]);
		return createStringValue(chopped);
	}

	// WTF. Should never be reached.
	return get_handle();
}

DEFINE_LINK_FACTORY(DecimateLink, DECIMATE_LINK);

// ============================================================
