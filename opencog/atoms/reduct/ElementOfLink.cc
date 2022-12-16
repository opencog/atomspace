/*
 * opencog/atoms/reduct/ElementOfLink.cc
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/reduct/NumericFunctionLink.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include "ElementOfLink.h"

using namespace opencog;

ElementOfLink::ElementOfLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t)
{
	init();
}

ElementOfLink::ElementOfLink(const Handle& a, const Handle& b)
    : Link({a, b}, ELEMENT_OF_LINK)
{
	init();
}

void ElementOfLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, ELEMENT_OF_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a ElementOfLink");

	size_t nargs = _outgoing.size();
	if (2 != nargs)
		throw InvalidParamException(TRACE_INFO,
			"ElementOfLink expects two arguments, got %s",
			to_string().c_str());
}

// ============================================================

ValuePtr ElementOfLink::execute(AtomSpace* as, bool silent)
{
	// The vector we're planning on cutting down.
	ValuePtr vi(NumericFunctionLink::get_value(as, silent, _outgoing[1]));

	// Lets see what kind of index list this is. We accept two kinds:
	// A NumberNode and a FloatValue.
	// get_value() causes execution to happen on the arguments
	ValuePtr vm(NumericFunctionLink::get_value(as, silent, _outgoing[0]));
	Type mtype = vm->get_type();

	// Perhaps its a NumberNode?
	if (NUMBER_NODE == mtype)
	{
		NumberNodePtr nnp = NumberNodeCast(vm);
		return do_execute(nnp->value(), vi);
	}

	if (FLOAT_VALUE == mtype)
	{
		FloatValuePtr fvp = FloatValueCast(vm);
		return do_execute(fvp->value(), vi);
	}

	throw SyntaxException(TRACE_INFO,
		"Index list must be a list of numbers! Got %s",
		vm->to_string().c_str());
}

ValuePtr ElementOfLink::do_execute(const std::vector<double>& vindex,
                                   const ValuePtr& vi)
{
	Type vitype = vi->get_type();

	// Handle the various value types. Try the most likely ones first.
	// Is it a Link?
	if (vi->is_link())
	{
		const HandleSeq& oset(HandleCast(vi)->getOutgoingSet());
		HandleSeq chopped;
		for (double d : vindex)
			chopped.push_back(oset.at((int)(d+0.5)));
		return createLink(chopped, vitype);
	}

	// A vector of values or atoms.
	if (nameserver().isA(vitype, LINK_VALUE))
	{
		const std::vector<ValuePtr>& lvec(LinkValueCast(vi)->value());
		std::vector<ValuePtr> chopped;
		for (double d : vindex)
			chopped.push_back(lvec.at((int)(d+0.5)));
		return createLinkValue(chopped);
	}

	// If its a float value, it's a vector.
	if (nameserver().isA(vitype, FLOAT_VALUE))
	{
		const std::vector<double>& dvec(FloatValueCast(vi)->value());
		std::vector<double> chopped;
		for (double d : vindex)
			chopped.push_back(dvec.at((int)(d+0.5)));
		return createFloatValue(chopped);
	}

	// If its a plain number, assume it's a vector.
	if (NUMBER_NODE == vitype)
	{
		const std::vector<double>& dvec(NumberNodeCast(vi)->value());
		std::vector<double> chopped;
		for (double d : vindex)
			chopped.push_back(dvec.at((int)(d+0.5)));
		return createNumberNode(chopped);
	}

	// A vector of strings
	if (nameserver().isA(vitype, STRING_VALUE))
	{
		const std::vector<std::string>& svec(StringValueCast(vi)->value());
		std::vector<std::string> chopped;
		for (double d : vindex)
			chopped.push_back(svec.at((int)(d+0.5)));
		return createStringValue(chopped);
	}

	// A vector of bools.
	if (nameserver().isA(vitype, BOOL_VALUE))
	{
		const std::vector<bool>& bvec(BoolValueCast(vi)->value());
		std::vector<bool> chopped;
		for (double d : vindex)
			chopped.push_back(bvec.at((int)(d+0.5)));
		return createBoolValue(chopped);
	}

	// WTF. Should never be reached.
	return get_handle();
}

DEFINE_LINK_FACTORY(ElementOfLink, ELEMENT_OF_LINK);

// ============================================================
