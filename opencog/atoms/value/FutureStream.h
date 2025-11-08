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
#include <opencog/atoms/value/LinkValue.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * FutureStream will eexecute the stored list of Atoms, to obtain
 * a fresh list of Values, every time it is queried for data.
 */
class FutureStream
	: public LinkValue
{
protected:
	FutureStream(Type t) : LinkValue(t) {}

	void init(void);
	virtual void update() const;
	HandleSeq _formula;
	AtomSpace* _scratch;

public:
	FutureStream(const Handle&);
	FutureStream(const HandleSeq&&);
	FutureStream(const ValueSeq&);
	virtual ~FutureStream() {}

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;

	/** Returns true if two values are equal. */
	virtual bool operator==(const Value&) const;
};

VALUE_PTR_DECL(FutureStream);
CREATE_VALUE_DECL(FutureStream);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_FUTURE_STREAM_H
