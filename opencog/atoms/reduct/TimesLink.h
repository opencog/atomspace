/*
 * opencog/atoms/reduct/TimesLink.h
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_TIMES_LINK_H
#define _OPENCOG_TIMES_LINK_H

#include <opencog/atoms/reduct/ArithmeticLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The TimesLink implements the arithmetic operation of "times".
 */
class TimesLink : public ArithmeticLink
{
protected:
	static Handle one;
	ValuePtr kons(AtomSpace*, bool,
	              const ValuePtr&, const ValuePtr&) const;

	void init(void);

public:
	TimesLink(const HandleSeq&&, Type=TIMES_LINK);
	TimesLink(const Handle& a, const Handle& b);

	TimesLink(const TimesLink&) = delete;
	TimesLink& operator=(const TimesLink&) = delete;

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<TimesLink> TimesLinkPtr;
static inline TimesLinkPtr TimesLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<TimesLink>(a); }
static inline TimesLinkPtr TimesLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<TimesLink>(a); }

#define createTimesLink std::make_shared<TimesLink>

/** @}*/
}

#endif // _OPENCOG_TIMES_LINK_H
