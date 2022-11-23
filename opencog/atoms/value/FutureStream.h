/*
 * opencog/atoms/value/FutureStream.h
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
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

#ifndef _OPENCOG_FUTURE_STREAM_H
#define _OPENCOG_FUTURE_STREAM_H

#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/LinkValue.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * FutureStream will evaluate the stored Atom to obtain a fresh
 * Value, every time it is queried for data.
 */
class FutureStream
	: public LinkValue
{
protected:
	virtual void update() const;
	Handle _formula;
	AtomSpace* _as;

public:
	FutureStream(const Handle&);
	virtual ~FutureStream() {}

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;

	/** Returns true if two values are equal. */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<FutureStream> FutureStreamPtr;
static inline FutureStreamPtr FutureStreamCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<FutureStream>(a); }

template<typename ... Type>
static inline std::shared_ptr<FutureStream> createFutureStream(Type&&... args)
{
	return std::make_shared<FutureStream>(std::forward<Type>(args)...);
}


/** @}*/
} // namespace opencog

#endif // _OPENCOG_FUTURE_STREAM_H
