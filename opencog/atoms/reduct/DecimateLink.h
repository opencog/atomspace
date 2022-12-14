/*
 * opencog/atoms/reduct/DecimateLink.h
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_DECIMATE_LINK_H
#define _OPENCOG_DECIMATE_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The DecimateLink implements the masking out of elements of a vector.
 * (Decimate (BoolValue 1 0 1 1) (Vector a b c d)) is just (Vector a c d)
 * The vector elements to keep are given by a bit-mask.  Similar to
 * ElementOfLink, except that the ElementOfLink specifies the index of
 * elements to select.
 */
class DecimateLink : public Link
{
protected:
	void init(void);
	ValuePtr do_execute(const std::vector<bool>&, const ValuePtr&);

public:
	DecimateLink(const Handle&, const Handle&);
	DecimateLink(const HandleSeq&&, Type=DECIMATE_LINK);

	DecimateLink(const DecimateLink&) = delete;
	DecimateLink& operator=(const DecimateLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(DecimateLink)
#define createDecimateLink CREATE_DECL(DecimateLink)

/** @}*/
}

#endif // _OPENCOG_DECIMATE_LINK_H
