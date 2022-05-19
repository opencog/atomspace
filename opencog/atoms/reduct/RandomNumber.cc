/*
 * opencog/atoms/reduct/RandomNumber.cc
 *
 * Copyright (C) 2015,2021 Linas Vepstas
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
	: NumericFunctionLink(std::move(oset), t)
{
	init();
}

// ---------------------------------------------------------------

static double get_ran(double lb, double ub)
{
	// Linear algebra slope-intercept formula.
	return (ub - lb) * randy.randdouble() + lb;
}

/// RandomNumberLink always returns either a NumberNode, or a
/// set of NumberNodes.  This is in contrast to a RandomValue,
/// which always returns a vector of doubles.
ValuePtr RandomNumberLink::execute(AtomSpace *as, bool silent)
{
	ValueSeq reduction;

	ValuePtr result = apply_func (as, silent, _outgoing,
		get_ran, reduction);

	if (result) return result;

   // No numeric values available. Sorry!
	// Return the best-possible reduction that we did get.
	if (reduction[0]->is_atom() and reduction[1]->is_atom())
		return createRandomNumberLink(HandleSeq(
			{HandleCast(reduction[0]),
			HandleCast(reduction[1])}));

	// Unable to reduce at all. Just return the original atom.
	return get_handle();
}

DEFINE_LINK_FACTORY(RandomNumberLink, RANDOM_NUMBER_LINK);

/* ===================== END OF FILE ===================== */
