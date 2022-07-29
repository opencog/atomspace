/*
 * opencog/atoms/reduct/BoolOpLink.h
 *
 * Copyright (C) 2015,2018,2022 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_BOOL_OP_LINK_H
#define _OPENCOG_BOOL_OP_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The BoolOpLink implements the logical operators on vectors of bools.
 * OrderedLink version.
 */
class BoolOpLink : public Link
{
protected:
	void init(void);

public:
	BoolOpLink(const HandleSeq&&, Type);

	BoolOpLink(const BoolOpLink&) = delete;
	BoolOpLink& operator=(const BoolOpLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(BoolOpLink)
#define createBoolOpLink CREATE_DECL(BoolOpLink)

/** @}*/
}

#endif // _OPENCOG_BOOL_OP_LINK_H
