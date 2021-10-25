/*
 * opencog/atoms/reduct/NumericOutLink.h
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

#ifndef _OPENCOG_NUMERIC_OUT_LINK_H
#define _OPENCOG_NUMERIC_OUT_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The NumericOutLink implements the simple arithmetic operations.
 * It uses FoldLink to perform delta-reduction.
 */
class NumericOutLink : public FunctionLink
{
protected:
	void init();

	static const std::vector<double>* get_vector(AtomSpace*, bool,
		ValuePtr, Type&);
	static ValuePtr apply_func(AtomSpace*, bool, const Handle&,
		double (*)(double), ValuePtr&);
	static ValuePtr apply_func(AtomSpace*, bool, const HandleSeq&,
		double (*)(double, double), ValueSeq&);

public:
	NumericOutLink(const HandleSeq&&, Type=NUMERIC_OUT_LINK);

	NumericOutLink(const NumericOutLink&) = delete;
	NumericOutLink& operator=(const NumericOutLink&) = delete;

	static ValuePtr get_value(AtomSpace*, bool, ValuePtr);
};

typedef std::shared_ptr<NumericOutLink> NumericOutLinkPtr;
static inline NumericOutLinkPtr NumericOutLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<NumericOutLink>(a); }
static inline NumericOutLinkPtr NumericOutLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<NumericOutLink>(a); }

#define createNumericOutLink std::make_shared<NumericOutLink>

/** @}*/
}

#endif // _OPENCOG_NUMERIC_OUT_LINK_H
