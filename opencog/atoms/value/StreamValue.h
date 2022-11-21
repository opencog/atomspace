/*
 * opencog/atoms/value/StreamValue.h
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
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

#ifndef _OPENCOG_STREAM_VALUE_H
#define _OPENCOG_STREAM_VALUE_H

#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * StreamValues provide a base class for any kind of values
 * that can provide continuously-updating, dynamic stream of
 * floating-point data. They are meant to hold any kind of
 * rapidly-changing data encoded as floats, including video and
 * audio feeds, or other kinds of high-bandwidth data.
 *
 * This class itself doesn't "do anything" other than to provide
 * a base class.
 *
 * See also LinkStreamValue when the data is encoded as Atoms or
 * as other (non-floating-point) Values.
 */
class StreamValue
	: public FloatValue
{
protected:
	StreamValue(Type t) : FloatValue(t) {}

public:
	virtual ~StreamValue() {}

	/** Returns true if two atoms are equal.  */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<StreamValue> StreamValuePtr;
static inline StreamValuePtr StreamValueCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<StreamValue>(a); }

/** @}*/
} // namespace opencog

#endif // _OPENCOG_STREAM_VALUE_H
