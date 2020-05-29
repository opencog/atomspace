/*
 * opencog/atoms/value/LinkStreamValue.h
 *
 * Copyright (C) 2020 Linas Vepstas
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

#ifndef _OPENCOG_LINK_STREAM_VALUE_H
#define _OPENCOG_LINK_STREAM_VALUE_H

#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * LinkStreamValues provide a base class for rapidly time-varying
 * values.
 */
class LinkStreamValue
	: public LinkValue
{
protected:
	LinkStreamValue(Type t) : LinkValue(t) {}

public:
	virtual ~LinkStreamValue() {}

	/** Returns true if two atoms are equal.  */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<LinkStreamValue> LinkStreamValuePtr;
static inline LinkStreamValuePtr LinkStreamValueCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<LinkStreamValue>(a); }


/** @}*/
} // namespace opencog

#endif // _OPENCOG_LINK_STREAM_VALUE_H
