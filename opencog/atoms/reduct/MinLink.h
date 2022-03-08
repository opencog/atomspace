/*
 * opencog/atoms/reduct/MinLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_MIN_LINK_H
#define _OPENCOG_MIN_LINK_H

#include <opencog/atoms/reduct/NumericFunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The MinLink implements the arithmetic operation of "least of"
 * on a component-by-component level.
 */
class MinLink : public NumericFunctionLink
{
protected:
	void init(void);

public:
	MinLink(const HandleSeq&&, Type=MIN_LINK);

	MinLink(const MinLink&) = delete;
	MinLink& operator=(const MinLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(MinLink)
#define createMinLink CREATE_DECL(MinLink)

/** @}*/
}

#endif // _OPENCOG_MIN_LINK_H
