/*
 * RandomNumber.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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

#include <opencog/util/mt19937ar.h>

#include <opencog/atoms/core/NumberNode.h>

#include "RandomNumber.h"

using namespace opencog;

static MT19937RandGen randy(616432);

void RandomNumberLink::init()
{
	// Type must be as expected
	Type tscope = get_type();
	if (not nameserver().isA(tscope, RANDOM_NUMBER_LINK))
	{
		const std::string& tname = nameserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an RandomNumberLink, got %s", tname.c_str());
	}

	if (_outgoing.size() != 2)
		throw SyntaxException(TRACE_INFO,
			"Expecting a numerical min and max; got %s",
			to_string().c_str());
}

RandomNumberLink::RandomNumberLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	init();
}

// ---------------------------------------------------------------

static double get_dbl(AtomSpace* as, bool silent, const Handle& h)
{
	Type t = h->get_type();
	if (NUMBER_NODE == t)
	{
		NumberNodePtr na(NumberNodeCast(h));
		return na->get_value();
	}
	if (h->is_executable())
	{
		ValuePtr vp = h->execute(as, silent);
		if (vp->is_atom())
			return get_dbl(as, silent, HandleCast(vp));
		if (nameserver().isA(vp->get_type(), FLOAT_VALUE))
			return FloatValueCast(vp)->value().at(0);
	}
	throw SyntaxException(TRACE_INFO,
		"Expecting a number, got %s", h->to_string().c_str());
}

// The pattern matcher returns sets of atoms; if that set contains
// numbers or something that when executed, returns numbers, then
// unwrap it.
static std::vector<double> unwrap_set(AtomSpace *as, bool silent,
                                      const Handle& h)
{
	Type t = h->get_type();
	if (NUMBER_NODE == t)
	{
		NumberNodePtr na(NumberNodeCast(h));
		return std::vector<double>(1, na->get_value());
	}
	if (SET_LINK == t)
	{
		if (0 == h->get_arity())
			throw SyntaxException(TRACE_INFO,
				"Expecting a number, got the empty set!\n");
		std::vector<double> nums;
		for (const Handle& ho: h->getOutgoingSet())
		{
			nums.push_back(get_dbl(as, silent, ho));
		}
		return nums;
	}

	if (h->is_executable())
	{
		ValuePtr vp = h->execute(as, silent);
		if (vp->is_atom())
			return unwrap_set(as, silent, HandleCast(vp));

		if (nameserver().isA(vp->get_type(), FLOAT_VALUE))
			return FloatValueCast(vp)->value();
	}

	throw SyntaxException(TRACE_INFO,
		"Expecting a number, got this: %s",
			h->to_string().c_str());
	return std::vector<double>();
}

static Handle get_ran(double cept, double nmax)
{
	// Linear algebra slope-intercept formula.
	double slope = nmax - cept;
	return HandleCast(createNumberNode(slope * randy.randdouble() + cept));
}

/// RandomNumberLink always returns either a NumberNode, or a
/// set of NumberNodes.  This is in contrast to a RandomValue,
/// which always returns a vector of doubles.
ValuePtr RandomNumberLink::execute(AtomSpace *as, bool silent)
{
	std::vector<double> nmin(unwrap_set(as, silent, _outgoing[0]));
	std::vector<double> nmax(unwrap_set(as, silent, _outgoing[1]));

	size_t len = nmax.size();
	if (nmin.size() != len)
		throw SyntaxException(TRACE_INFO,
			"Unmatched number of bounds: %d vs. %d",
				nmin.size(), nmax.size());

	if (1 == len)
		return get_ran(nmin[0], nmax[0]);

	HandleSeq oset;
	for (size_t i=0; i< len; i++)
		oset.push_back(get_ran(nmin[i], nmax[i]));

	return ValuePtr(createLink(std::move(oset), SET_LINK));
}

DEFINE_LINK_FACTORY(RandomNumberLink, RANDOM_NUMBER_LINK);

/* ===================== END OF FILE ===================== */
