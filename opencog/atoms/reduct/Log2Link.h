/*
 * opencog/atoms/reduct/Log2Link.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_LOG2_LINK_H
#define _OPENCOG_LOG2_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The Log2Link implements the arithmetic operation of "greater
 * than" on a component-by-component level. That is,
 *    Log2 (a, b, c) (d, e, f) is just (a>d,  b>e, c>f).
 */
class Log2Link : public FunctionLink
{
protected:
	void init(void);

public:
	Log2Link(const Handle& a);
	Log2Link(const Handle& a, const Handle& b);
	Log2Link(const HandleSeq&&, Type=LOG2_LINK);

	Log2Link(const Log2Link&) = delete;
	Log2Link& operator=(const Log2Link&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<Log2Link> Log2LinkPtr;
static inline Log2LinkPtr Log2LinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<Log2Link>(a); }
static inline Log2LinkPtr Log2LinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<Log2Link>(a); }

#define createLog2Link std::make_shared<Log2Link>

/** @}*/
}

#endif // _OPENCOG_LOG2_LINK_H
