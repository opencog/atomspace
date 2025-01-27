/*
 * opencog/atoms/column/LinkColumn.h
 *
 * Copyright (C) 2015, 2022, 2025 Linas Vepstas
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

#ifndef _OPENCOG_LINK_COLUMN_H
#define _OPENCOG_LINK_COLUMN_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The LinkColumn returns a LinkValue vector of whatever it is
/// wrapping.
///
/// For example,
///
///     LinkColumn
///         Concept "a"
///         Concept "b"
///
/// will return a value vector of length two:
///
///     (LinkValue (Concept "a") (Concept "b"))
///
/// The intended use case is that this will be used with pattern
/// searches, to construct a list of columns.
class LinkColumn : public Link
{
protected:
	ValuePtr do_execute(AtomSpace*, bool);
	ValuePtr do_handle_loop(AtomSpace*, bool, const HandleSeq&);

public:
	LinkColumn(const HandleSeq&&, Type = LINK_COLUMN);
	LinkColumn(const LinkColumn&) = delete;
	LinkColumn& operator=(const LinkColumn&) = delete;

	virtual bool is_executable() const { return true; }

	// Return a pointer to LinkValue holding a list of Values.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(LinkColumn)
#define createLinkColumn CREATE_DECL(LinkColumn)

/** @}*/
}

#endif // _OPENCOG_LINK_COLUMN_H
