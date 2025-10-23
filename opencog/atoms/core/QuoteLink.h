/*
 * opencog/atoms/core/QuoteLink.h
 *
 * Copyright (C) 2025 OpenCog Foundation
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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_QUOTE_LINK_H
#define _OPENCOG_QUOTE_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The QuoteLink prevents evaluation of its contents during pattern
/// matching and other operations. When executed, it returns the quoted
/// content, removing the outermost Quote. If the content is an
/// UnquoteLink, both Quote and Unquote are removed (involution).
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
///     (Quote (Unquote (Concept "bar")))
///
/// when executed returns:
///
///     (Concept "bar")
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
