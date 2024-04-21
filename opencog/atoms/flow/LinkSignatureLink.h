/*
 * opencog/atoms/flow/LinkSignatureLink.h
 *
 * Copyright (C) 2015, 2022 Linas Vepstas
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

#ifndef _OPENCOG_LINK_SIGNATURE_LINK_H
#define _OPENCOG_LINK_SIGNATURE_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The LinkSignatureLink returns Value of the indicated type.
/// It is effectively a Link constructor.  It is a bit experimental
/// in form, and is used only in a few places.
/// It could be called LinkConstructorLink, except that it is also
/// used for pattern matching in assorted places.
///
/// For example,
///
///     LinkSignatureLink
///         TypeNode 'LinkValue
///         AtomA
///         AtomB
///
/// will return
///
///     (LinkValue (AtomA) (AtomB))
///
class LinkSignatureLink : public Link
{
private:
	Type _kind;

public:
	LinkSignatureLink(const HandleSeq&&, Type = LINK_SIGNATURE_LINK);
	LinkSignatureLink(const LinkSignatureLink&) = delete;
	LinkSignatureLink& operator=(const LinkSignatureLink&) = delete;

	Type get_kind(void) const { return _kind; }
	virtual bool is_executable(void) const { return true; }

	// Construct new Value or Link of the given kind.
	virtual ValuePtr execute(AtomSpace*, bool);

	// Construct new Value from arguments
	ValuePtr construct(const ValueSeq&&);
	ValuePtr construct(const HandleSeq&&);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(LinkSignatureLink)
#define createLinkSignatureLink CREATE_DECL(LinkSignatureLink)

/** @}*/
}

#endif // _OPENCOG_LINK_SIGNATURE_LINK_H
