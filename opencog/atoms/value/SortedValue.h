/*
 * opencog/atoms/value/SortedValue.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
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

#ifndef _OPENCOG_SORTED_VALUE_H
#define _OPENCOG_SORTED_VALUE_H

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/value/UnisetValue.h>
#include <opencog/atoms/flow/ValueShimLink.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * A LinkValue that maintains contents in sorted order.
 * Sort order is determined by the sort schema.
 * This must be executable, take exactly two arguments,
 * and must return a crisp bool value, indicating the order.
 */
class SortedValue
	: public UnisetValue
{
protected:
	Handle _schema;
	ValueShimLinkPtr _left_shim;
	ValueShimLinkPtr _right_shim;
	Handle _exout;
	Handle _source;
	AtomSpace* _scratch;

	void init(void);
	virtual void update() const override;
	virtual bool less(const Value& lhs, const Value& rhs) const override;

public:
	SortedValue(const Handle&);
	SortedValue(const HandleSeq&);
	virtual ~SortedValue();

	virtual void add(const ValuePtr&) override;
	virtual void add(ValuePtr&&) override;
};

VALUE_PTR_DECL(SortedValue);
CREATE_VALUE_DECL(SortedValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_SORTED_VALUE_H
