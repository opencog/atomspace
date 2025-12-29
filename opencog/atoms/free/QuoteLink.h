/*
 * opencog/atoms/core/QuoteLink.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#ifndef _OPENCOG_QUOTE_LINK_H
#define _OPENCOG_QUOTE_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The QuoteLink is used to prevent evaluation of its contents during
/// pattern matching and during beta reduction. To unwrap the quoted
/// expression ("to consume the quotes"), it is enough to execute the
/// QuoteLink: the quoted content will be returned. Matching nested
/// Unquotes are consumed in the process.
///
/// For example:
///
///     (Quote (Concept "foo"))
///
/// when executed returns:
///
///     (Concept "foo")
///
/// And:
///
///     (Quote (List (Unquote (Concept "bar"))))
///
/// when executed returns:
///
///     (List (Concept "bar"))
///
class QuoteLink : public Link
{
public:
	QuoteLink(const HandleSeq&&, Type=QUOTE_LINK);
	QuoteLink(const QuoteLink&) = delete;
	QuoteLink& operator=(const QuoteLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(QuoteLink)
#define createQuoteLink CREATE_DECL(QuoteLink)

/** @}*/
}

#endif // _OPENCOG_QUOTE_LINK_H
