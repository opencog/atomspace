/*
 * opencog/atoms/reduct/MaxLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_MAX_LINK_H
#define _OPENCOG_MAX_LINK_H

#include <opencog/atoms/reduct/NumericFunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The MaxLink implements the arithmetic operation of "greatest of"
 * on a component-by-component level.
 */
class MaxLink : public NumericFunctionLink
{
protected:
	void init(void);

public:
	MaxLink(const HandleSeq&&, Type=MAX_LINK);

	MaxLink(const MaxLink&) = delete;
	MaxLink& operator=(const MaxLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<MaxLink> MaxLinkPtr;
static inline MaxLinkPtr MaxLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<MaxLink>(a); }
static inline MaxLinkPtr MaxLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<MaxLink>(a); }

#define createMaxLink std::make_shared<MaxLink>

/** @}*/
}

#endif // _OPENCOG_MAX_LINK_H
