/*
 * opencog/atoms/reduct/NumericFunctionLink.cc
 *
 * Copyright (C) 2015, 2018, 2021 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <limits>

#include <opencog/util/mt19937ar.h>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include "NumericFunctionLink.h"

using namespace opencog;

NumericFunctionLink::NumericFunctionLink(const HandleSeq&& oset, Type t)
    : FunctionLink(std::move(oset), t)
{
	init();
}

void NumericFunctionLink::init(void)
{
	Type t = get_type();
	if (NUMERIC_FUNCTION_LINK == t)
		throw InvalidParamException(TRACE_INFO,
			"NumericFunctionLinks are private and cannot be instantiated.");

	if (not nameserver().isA(t, NUMERIC_FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an NumericFunctionLink");

	if (HEAVISIDE_LINK == t and 1 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO, "HeavisideLink expects one argument");
	else if (LOG2_LINK == t and 1 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO, "Log2Link expects one argument");

	else if (POW_LINK == t and 2 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO, "PowLink expects two arguments");

	else if (RANDOM_NUMBER_LINK == t and 2 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO, "RandomNumberLink expects two arguments");
}

// ===========================================================

/// Generic utility -- execute the argument, and return the result
/// of the execution.
ValuePtr NumericFunctionLink::get_value(AtomSpace* as, bool silent, ValuePtr vptr)
{
	if (DEFINED_SCHEMA_NODE == vptr->get_type())
	{
		vptr = DefineLink::get_definition(HandleCast(vptr));
	}
	while (vptr->is_atom())
	{
		Handle h(HandleCast(vptr));
		if (not h->is_executable()) break;

		ValuePtr red(h->execute(as, silent));

		// It would probably be better to throw a silent exception, here?
		if (nullptr == red) return vptr;
		if (*red == *vptr) return vptr;
		vptr = red;

		// The executable function might be a GetLink, which returns
		// a SetLink of results. If the SetLink is wrapping only one
		// atom, then unwrap it and return that value. If it contains
		// more than one atom, we don't know what to do.
		if (SET_LINK == vptr->get_type())
		{
			Handle setl(HandleCast(vptr));
			if (1 == setl->get_arity())
				vptr = setl->getOutgoingAtom(0);
		}
	}
	return vptr;
}

// ===========================================================

/// Generic utility -- convert the argument to a vector of doubles,
/// if possible.  Reutnr nullptr if not possible.
const std::vector<double>*
NumericFunctionLink::get_vector(AtomSpace* as, bool silent,
                           ValuePtr vptr, Type& t)
{
	t = vptr->get_type();

	bool is_fv = nameserver().isA(t, FLOAT_VALUE);
	bool is_nu = (NUMBER_NODE == t);

	if (not is_fv and not is_nu) return nullptr;

	if (is_nu)
		return & NumberNodeCast(vptr)->value();
	if (is_fv)
		return & FloatValueCast(vptr)->value();

	return nullptr; // not reached
}

// ============================================================

/// Generic utility -- execute the Handle, and, if that returned
/// a vector of doubles, then apply the function to them.
/// If there wasn't a numeric vectors, return a null pointer.
/// In this last case, the result of reduction is returned
/// in `reduction`
ValuePtr
NumericFunctionLink::apply_func(AtomSpace* as, bool silent,
                           const Handle& arg,
                           double (*fun)(double),
                           ValuePtr& vx)
{
	// get_value() causes execution.
	vx = get_value(as, silent, arg);

	// get_vector gets numeric values, if possible.
	Type vxtype;
	const std::vector<double>* xvec = get_vector(as, silent, vx, vxtype);

	// No numeric values available. Sorry!
	if (nullptr == xvec or 0 == xvec->size())
		return nullptr;

	std::vector<double> funvec;
	size_t sz = xvec->size();
	for (size_t i=0; i<sz; i++)
		funvec.push_back(fun(xvec->operator[](i)));

	if (NUMBER_NODE == vxtype)
		return createNumberNode(funvec);

	return createFloatValue(funvec);
}

// ============================================================

/// Generic utility -- execute the HandleSeq, and, if that returned
/// vectors of doubles, then apply the function to them.
/// If there weren't any vectors, return a null pointer.
/// In this last case, the result of reduction is returned
/// in `reduction`
ValuePtr
NumericFunctionLink::apply_func(AtomSpace* as, bool silent,
                           const HandleSeq& args,
                           double (*fun)(double, double),
                           ValueSeq& reduction)
{
	// get_value() causes execution.
	ValuePtr vx(get_value(as, silent, args[0]));
	ValuePtr vy(get_value(as, silent, args[1]));

	// get_vector gets numeric values, if possible.
	Type vxtype;
	const std::vector<double>* xvec = get_vector(as, silent, vx, vxtype);

	Type vytype;
	const std::vector<double>* yvec = get_vector(as, silent, vy, vytype);

	// No numeric values available. Sorry!
	if (nullptr == xvec or nullptr == yvec or
	    0 == xvec->size() or 0 == yvec->size())
	{
		reduction.push_back(vx);
		reduction.push_back(vy);
		return nullptr;
	}

	std::vector<double> funvec;
	if (1 == xvec->size())
	{
		double x = xvec->back();
		for (double y : *yvec)
			funvec.push_back(fun(x, y));
	}
	else if (1 == yvec->size())
	{
		double y = yvec->back();
		for (double x : *xvec)
			funvec.push_back(fun(x, y));
	}
	else
	{
		size_t sz = std::min(xvec->size(), yvec->size());
		for (size_t i=0; i<sz; i++)
			funvec.push_back(fun(xvec->operator[](i), yvec->operator[](i)));
	}

	if (NUMBER_NODE == vxtype and NUMBER_NODE == vytype)
		return createNumberNode(funvec);

	return createFloatValue(funvec);
}

// ============================================================

static double impulse(double x) { return 1-std::signbit(x); }

static MT19937RandGen randy(616432);
static double get_ran(double lb, double ub)
{
	// Linear algebra slope-intercept formula.
	return (ub - lb) * randy.randdouble() + lb;
}

/// The various NumericFunctionLinks implement various numeric
/// functions on vector arguments (i.e. on FloatValues and
/// NumberNodes, both of which hold float pt vectors by default.)
///
/// ----
/// The HeavisideLink implements the arithmetic operation of "greater
/// than" on a component-by-component level. That is,
///    Heaviside (a, b, c) (d, e, f) is just (a>d,  b>e, c>f).
/// where the comparison is 1.0 if true, else 0.0.
/// Note it returns a FloatValue and NOT a BoolValue!
///
/// ----
/// The Log2Link implements the elementary function of
/// logarithm base two. That is,
///    Log2 (a, b, c) evaluates to (log2(a), log2(b), log2(c)).
///
/// ----
/// The RandomNumberLink returns a NumberNode that lies within the
/// min-max range, using a uniform distribution.
///
/// For example,
///
///     RandomNumberLink
///         NumberNode 0.1
///         NumberNode 0.5
///
/// will return a random number between 0.1 ad 0.5
///
/// It always returns either a NumberNode, or a set of NumberNodes.
/// This is in contrast to a RandomValue, which always returns a
/// vector of doubles (a FloatValue).
///
/// ----
/// The PowLink implements the arithmetic operation of raising
/// an argument to a power. If both arguments are vectors, then
/// they need to be the same size, and the power is computed
/// component by component. That is
///    Pow (a, b, c) (d, e, f) is just (a**d,  b**e, c**f).
/// If one of the arguments is a scalar, then that scalar is applied:
///    Pow (a, b, c) n is just (a**n,  b**n, c**n).
///    Pow a (p, q, r) is just (a**p,  a**q, a**r).

ValuePtr NumericFunctionLink::execute_unary(AtomSpace* as, bool silent)
{
	ValuePtr reduction;
	ValuePtr result;

	Type t = get_type();
	if (LOG2_LINK == t)
		result = apply_func(as, silent, _outgoing[0], log2, reduction);
	else if (HEAVISIDE_LINK == t)
		result = apply_func(as, silent, _outgoing[0], impulse, reduction);
	else
		throw InvalidParamException(TRACE_INFO,
			"Internal Error: unhandled derived type!");

	if (result) return result;

	// No numeric values available. Sorry!
	// Return the best-possible reduction that we did get.
	if (reduction->is_atom())
		return createNumericFunctionLink(
			HandleSeq({HandleCast(reduction)}), t);

	// Unable to reduce at all. Just return the original atom.
	return get_handle();
}

ValuePtr NumericFunctionLink::execute_binary(AtomSpace *as, bool silent)
{
	ValueSeq reduction;
	ValuePtr result;

	Type t = get_type();
	if (RANDOM_NUMBER_LINK == t)
		result = apply_func(as, silent, _outgoing, get_ran, reduction);
	else if (POW_LINK == t)
		result = apply_func(as, silent, _outgoing, pow, reduction);
	else
		throw InvalidParamException(TRACE_INFO,
			"Internal Error: unhandled derived type!");

	if (result) return result;

   // No numeric values available. Sorry!
	// Return the best-possible reduction that we did get.
	if (reduction[0]->is_atom() and reduction[1]->is_atom())
		return createNumericFunctionLink(HandleSeq(
			{HandleCast(reduction[0]),
			HandleCast(reduction[1])}), t);

	// Unable to reduce at all. Just return the original atom.
	return get_handle();
}

ValuePtr NumericFunctionLink::execute(AtomSpace* as, bool silent)
{
	if (1 == _outgoing.size())
		return execute_unary(as, silent);
	return execute_binary(as, silent);
}

DEFINE_LINK_FACTORY(NumericFunctionLink, NUMERIC_FUNCTION_LINK);

// ===========================================================
