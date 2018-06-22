/*
 * opencog/atoms/proto/StreamValue.h
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

#include <vector>
#include <opencog/atoms/proto/FloatValue.h>
#include <opencog/atoms/proto/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * StreamValues provide an example of streaming data.
 */
class StreamValue
	: public FloatValue
{
protected:
	StreamValue(Type t) : FloatValue(t) {}

public:
	virtual ~StreamValue() {}

	/** Returns true if two atoms are equal.  */
	virtual bool operator==(const ProtoAtom&) const;
};

typedef std::shared_ptr<StreamValue> StreamValuePtr;
static inline StreamValuePtr StreamValueCast(ProtoAtomPtr& a)
	{ return std::dynamic_pointer_cast<StreamValue>(a); }



/** @}*/
} // namespace opencog

#endif // _OPENCOG_STREAM_VALUE_H
