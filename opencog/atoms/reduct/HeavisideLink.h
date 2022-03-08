/*
 * opencog/atoms/reduct/HeavisideLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_HEAVISIDE_LINK_H
#define _OPENCOG_HEAVISIDE_LINK_H

#include <opencog/atoms/reduct/NumericFunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The HeavisideLink implements the arithmetic operation of "greater
 * than" on a component-by-component level. That is,
 *    Heaviside (a, b, c) (d, e, f) is just (a>d,  b>e, c>f).
 */
class HeavisideLink : public NumericFunctionLink
{
protected:
	void init(void);

public:
	HeavisideLink(const Handle& a);
	HeavisideLink(const Handle& a, const Handle& b);
	HeavisideLink(const HandleSeq&&, Type=HEAVISIDE_LINK);

	HeavisideLink(const HeavisideLink&) = delete;
	HeavisideLink& operator=(const HeavisideLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(HeavisideLink)
#define createHeavisideLink CREATE_DECL(HeavisideLink)

/** @}*/
}

#endif // _OPENCOG_HEAVISIDE_LINK_H
