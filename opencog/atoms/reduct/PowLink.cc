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
    : FunctionLink(std::move(oset), t)
{
	init();
}

PowLink::PowLink(const Handle& a, const Handle& b)
    : FunctionLink({a, b}, POW_LINK)
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
	// ArithmeticLink::get_value causes execution.
	ValuePtr vx(ArithmeticLink::get_value(as, silent, _outgoing[0]));
	ValuePtr vy(ArithmeticLink::get_value(as, silent, _outgoing[1]));

	// get_vector gets numeric values, if possible.
	Type vxtype;
	const std::vector<double>* xvec =
		ArithmeticLink::get_vector(as, silent, vx, vxtype);

	Type vytype;
	const std::vector<double>* yvec =
		ArithmeticLink::get_vector(as, silent, vy, vytype);

	// No numeric values available. Sorry!
	if (nullptr == xvec or nullptr == yvec or
	    0 == xvec->size() or 0 == yvec->size())
	{
		// If it did not fully reduce, then return the best-possible
		// reduction that we did get.
		if (vx->is_atom() and vy->is_atom())
			return createPowLink(HandleCast(vx), HandleCast(vy));

		// Unable to reduce at all. Just return the original atom.
		return get_handle();
	}

	std::vector<double> powvec;
	if (1 == xvec->size())
	{
		double x = xvec->back();
		for (double y : *yvec)
			powvec.push_back(pow(x,y));
	}
	else if (1 == yvec->size())
	{
		double y = yvec->back();
		for (double x : *xvec)
			powvec.push_back(pow(x,y));
	}
	else
	{
		size_t sz = std::min(xvec->size(), yvec->size());
		for (size_t i=0; i<sz; i++)
		{
			powvec.push_back(pow(xvec->operator[](i), yvec->operator[](i)));
		}
	}

	if (NUMBER_NODE == vxtype and NUMBER_NODE == vytype)
		return createNumberNode(powvec);

	return createFloatValue(powvec);
}

DEFINE_LINK_FACTORY(PowLink, POW_LINK);

// ============================================================
