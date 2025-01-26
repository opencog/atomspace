/*
 * FloatColumn.cc
 *
 * Copyright (C) 2015, 2022, 2025 Linas Vepstas
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

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/reduct/NumericFunctionLink.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/FloatValue.h>

#include "FloatColumn.h"

using namespace opencog;

FloatColumn::FloatColumn(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, FLOAT_COLUMN))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a FloatColumn, got %s", tname.c_str());
	}

	size_t sz = _outgoing.size();

	if (1 != sz)
		throw InvalidParamException(TRACE_INFO,
			"FloatColumn expects one arg, got %lu", sz);
}

// ---------------------------------------------------------------

/// Return a FloatValue vector.
ValuePtr FloatColumn::do_execute(AtomSpace* as, bool silent)
{
	// If the given Atom is executable, then execute it.
	Handle base(_outgoing[0]);
	if (base->is_executable())
	{
		ValuePtr vp(base->execute(as, silent));
		if (vp->is_atom())
			base = HandleCast(vp);
		else
		{
#if 0
			if (not vp->is_type(LINK_VALUE))
				return createStringValue(vp->to_string());

			// If we are here, we've got a LinkValue
			std::vector<std::string> svec;
			svec.reserve(vp->size());
			for (const ValuePtr& v : LinkValueCast(vp)->value())
				svec.push_back(v->to_short_string());
			return createStringValue(std::move(svec));
#endif
		}
	}

	// If we are here, then base is an atom.
	if (base->is_type(NUMBER_NODE))
	{
		std::vector<double> nums(NumberNodeCast(base)->value());
		return createFloatValue(std::move(nums));
	}

	// If we are here, then base is an link. Expect
	// it to contain things that evaluate to a double
	std::vector<double> dvec;
	dvec.reserve(base->get_arity());
	for (const Handle& h : base->getOutgoingSet())
	{
		ValuePtr vp(NumericFunctionLink::get_value(as, silent, h));
		if (1 != vp->size())
			throw RuntimeException(TRACE_INFO,
				"Expecting exactly one number per item, got %lu\n",
				vp->size());
		if (vp->is_type(FLOAT_VALUE))
			dvec.push_back(FloatValueCast(vp)->value()[0]);
		else if (vp->is_type(NUMBER_NODE))
			dvec.push_back(NumberNodeCast(vp)->get_value());
		else
			throw RuntimeException(TRACE_INFO,
				"Expecting numeric value, got %s\n",
				vp->to_string());
	}

	return createFloatValue(std::move(dvec));
}

// ---------------------------------------------------------------

/// Return a FloatValue vector.
ValuePtr FloatColumn::execute(AtomSpace* as, bool silent)
{
	return do_execute(as, silent);
}

DEFINE_LINK_FACTORY(FloatColumn, FLOAT_COLUMN)

/* ===================== END OF FILE ===================== */
