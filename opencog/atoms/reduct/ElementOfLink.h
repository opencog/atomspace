/*
 * opencog/atoms/reduct/ElementOfLink.h
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_ELEMENT_OF_LINK_H
#define _OPENCOG_ELEMENT_OF_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The ElementOfLink implements the selection of elements out of a vector
 *    (ElementOf (Number 0 2 3) (Vector a b c d)) is just (Vector a c d)
 * The indexes to keep are listed in the first argument. Similar to
 * DecimateLink, except DecimateLink uses a bitmask.
 */
class ElementOfLink : public Link
{
protected:
	void init(void);
	ValuePtr do_execute(const std::vector<double>&, const ValuePtr&);

public:
	ElementOfLink(const Handle&, const Handle&);
	ElementOfLink(const HandleSeq&&, Type=ELEMENT_OF_LINK);

	ElementOfLink(const ElementOfLink&) = delete;
	ElementOfLink& operator=(const ElementOfLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ElementOfLink)
#define createElementOfLink CREATE_DECL(ElementOfLink)

/** @}*/
}

#endif // _OPENCOG_ELEMENT_OF_LINK_H
