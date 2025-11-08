/*
 * opencog/atoms/value/FlatStream.h
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
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

#ifndef _OPENCOG_FLAT_STREAM_H
#define _OPENCOG_FLAT_STREAM_H

#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/LinkValue.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * FlatStream will evaluate the stored Atom to obtain a fresh
 * Value, every time it is queried for data.
 */
class FlatStream
	: public LinkValue
{
protected:
	FlatStream(Type t) : LinkValue(t) {}

	void init(const ValuePtr&);
	virtual void update() const;
	LinkValuePtr _source;
	mutable LinkValuePtr _collection;
	mutable size_t _index;

public:
	FlatStream(const Handle&);
	FlatStream(const ValuePtr&);
	virtual ~FlatStream() {}

	virtual std::string to_string(const std::string& indent = "") const;
};

VALUE_PTR_DECL(FlatStream);
CREATE_VALUE_DECL(FlatStream);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_FLAT_STREAM_H
