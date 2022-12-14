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
 * The ElementOfLink implements a sum over a numeric series.
 *    (ElementOf (Number a b c))  is just a+b+c.
 */
class ElementOfLink : public Link
{
protected:
	void init(void);
	ValuePtr do_execute(const std::vector<bool>&, const ValuePtr&);

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
