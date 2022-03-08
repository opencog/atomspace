/*
 * opencog/atoms/reduct/AccumulateLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_ACCUMULATE_LINK_H
#define _OPENCOG_ACCUMULATE_LINK_H

#include <opencog/atoms/reduct/NumericFunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The AccumulateLink implements a sum over a numeric series.
 *    (Accumulate (Number a b c))  is just a+b+c.
 */
class AccumulateLink : public NumericFunctionLink
{
protected:
	void init(void);

public:
	AccumulateLink(const Handle& a);
	AccumulateLink(const Handle& a, const Handle& b);
	AccumulateLink(const HandleSeq&&, Type=ACCUMULATE_LINK);

	AccumulateLink(const AccumulateLink&) = delete;
	AccumulateLink& operator=(const AccumulateLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(AccumulateLink)
#define createAccumulateLink std::make_shared<AccumulateLink>

/** @}*/
}

#endif // _OPENCOG_ACCUMULATE_LINK_H
