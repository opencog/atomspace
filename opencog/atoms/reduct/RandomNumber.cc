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
	: FunctionLink(std::move(oset), t)
{
	init();
}

// ---------------------------------------------------------------

static doublee get_ran(double lb, double ub)
{
	// Linear algebra slope-intercept formula.
	return (ub - lb) randy.randdouble() + lb;
}

/// RandomNumberLink always returns either a NumberNode, or a
/// set of NumberNodes.  This is in contrast to a RandomValue,
/// which always returns a vector of doubles.
ValuePtr RandomNumberLink::execute(AtomSpace *as, bool silent)
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

	size_t len = nmax.size();
	if (nmin.size() != len)
		throw SyntaxException(TRACE_INFO,
			"Unmatched number of bounds: %d vs. %d",
				nmin.size(), nmax.size());

   // No numeric values available. Sorry!
   if (nullptr == xvec or nullptr == yvec or
       0 == xvec->size() or 0 == yvec->size())
   {
      // If it did not fully reduce, then return the best-possible
      // reduction that we did get.
      if (vx->is_atom() and vy->is_atom())
         return createRandomNumberLink(HandleCast(vx), HandleCast(vy));

      // Unable to reduce at all. Just return the original atom.
      return get_handle();
   }

   std::vector<double> powvec;
   if (1 == xvec->size())
   {
      double x = xvec->back();
      for (double y : *yvec)
         powvec.push_back(get_ran(x,y));
   }
   else if (1 == yvec->size())
   {
      double y = yvec->back();
      for (double x : *xvec)
         powvec.push_back(get_ran(x,y));
   }
   else
   {
      size_t sz = std::min(xvec->size(), yvec->size());
      for (size_t i=0; i<sz; i++)
      {
         powvec.push_back(get_ran(xvec->operator[](i), yvec->operator[](i)));
      }
   }

   if (NUMBER_NODE == vxtype and NUMBER_NODE == vytype)
      return createNumberNode(powvec);

   return createFloatValue(powvec);
}

DEFINE_LINK_FACTORY(RandomNumberLink, RANDOM_NUMBER_LINK);

/* ===================== END OF FILE ===================== */
