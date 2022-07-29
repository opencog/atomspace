/*
 * opencog/atoms/reduct/NumericFunctionLink.h
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
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

#ifndef _OPENCOG_NUMERIC_FUNCTION_LINK_H
#define _OPENCOG_NUMERIC_FUNCTION_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The NumericFunctionLink implements the simple arithmetic operations.
 * It uses FoldLink to perform delta-reduction.
 */
class NumericFunctionLink : public FunctionLink
{
protected:
	void init();
	ValuePtr execute_unary(AtomSpace*, bool);
	ValuePtr execute_binary(AtomSpace*, bool);

	static const std::vector<double>* get_vector(AtomSpace*, bool,
		ValuePtr, Type&);
	static ValuePtr apply_func(AtomSpace*, bool, const Handle&,
		double (*)(double), ValuePtr&);
	static ValuePtr apply_func(AtomSpace*, bool, const HandleSeq&,
		double (*)(double, double), ValueSeq&);

public:
	NumericFunctionLink(const HandleSeq&&, Type);

	NumericFunctionLink(const NumericFunctionLink&) = delete;
	NumericFunctionLink& operator=(const NumericFunctionLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);

	static ValuePtr get_value(AtomSpace*, bool, ValuePtr);
	static Handle factory(const Handle&);
};

LINK_PTR_DECL(NumericFunctionLink)
#define createNumericFunctionLink CREATE_DECL(NumericFunctionLink)

/** @}*/
}

#endif // _OPENCOG_NUMERIC_FUNCTION_LINK_H
