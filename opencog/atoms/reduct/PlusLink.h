/*
 * opencog/atoms/reduct/PlusLink.h
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_PLUS_LINK_H
#define _OPENCOG_PLUS_LINK_H

#include <opencog/atoms/reduct/ArithmeticLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The PlusLink implements the arithmetic operation of "plus"
 */
class PlusLink : public ArithmeticLink
{
protected:
	static Handle zero;
	virtual ValuePtr kons(AtomSpace*, bool,
	                      const ValuePtr&, const ValuePtr&) const;

	void init(void);

public:
	PlusLink(const Handle& a, const Handle& b);
	PlusLink(const HandleSeq&&, Type=PLUS_LINK);

	PlusLink(const PlusLink&) = delete;
	PlusLink& operator=(const PlusLink&) = delete;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(PlusLink)
#define createPlusLink std::make_shared<PlusLink>

/** @}*/
}

#endif // _OPENCOG_PLUS_LINK_H
