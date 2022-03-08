/*
 * opencog/atoms/reduct/PowLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_POW_LINK_H
#define _OPENCOG_POW_LINK_H

#include <opencog/atoms/reduct/NumericFunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The PowLink implements the arithmetic operation of raising
 * an argument to a power. If both arguments are vectors, then
 * they need to be the same size, and the power is computed
 * component by component. That is
 *    Pow (a, b, c) (d, e, f) is just (a**d,  b**e, c**f).
 * If one of the arguments is a scalar, then that scalar is applied:
 *    Pow (a, b, c) n is just (a**n,  b**n, c**n).
 *    Pow a (p, q, r) is just (a**p,  a**q, a**r).
 */
class PowLink : public NumericFunctionLink
{
protected:
	void init(void);

public:
	PowLink(const Handle& a, const Handle& b);
	PowLink(const HandleSeq&&, Type=POW_LINK);

	PowLink(const PowLink&) = delete;
	PowLink& operator=(const PowLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(PowLink)
#define createPowLink CREATE_DECL(PowLink)

/** @}*/
}

#endif // _OPENCOG_POW_LINK_H
