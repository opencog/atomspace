/*
 * opencog/atoms/column/TransposeColumn.h
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

#ifndef _OPENCOG_TRANSPOSE_COLUMN_H
#define _OPENCOG_TRANSPOSE_COLUMN_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TransposeColumn returns the transpose of collection of rows.
///
/// For example,
///
///     TransposeColumn
///         Number 1 2 3
///         Number 4 5 6
///
/// will return a LinkValue of length three:
///
///     (LinkValue
///         (FloatValue 1.0 4.0)
///         (FloatValue 2.0 5.0)
///         (FloatValue 3.0 6.0))
///
/// The intended use case is in combination with pattern searches,
/// to obtain column vectors from a list of individual results.
class TransposeColumn : public Link
{
protected:
	ValuePtr do_execute(AtomSpace*, bool);
	ValuePtr do_handle_loop(AtomSpace*, bool, const HandleSeq&);
	ValuePtr do_value_loop(AtomSpace*, bool, const ValueSeq&);

public:
	TransposeColumn(const HandleSeq&&, Type = TRANSPOSE_COLUMN);
	TransposeColumn(const TransposeColumn&) = delete;
	TransposeColumn& operator=(const TransposeColumn&) = delete;

	virtual bool is_executable() const { return true; }

	// Return a pointer to LinkValue holding multiple columns.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(TransposeColumn)
#define createTransposeColumn CREATE_DECL(TransposeColumn)

/** @}*/
}

#endif // _OPENCOG_TRANSPOSE_COLUMN_H
