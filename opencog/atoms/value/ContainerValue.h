/*
 * opencog/atoms/value/ContainerValue.h
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

#ifndef _OPENCOG_CONTAINER_VALUE_H
#define _OPENCOG_CONTAINER_VALUE_H

#include <opencog/atoms/value/LinkStreamValue.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * ContainerValues provide a thread-safe containers for Values. They
 * provide a producer-consumer API, where the produced values are to
 * added to the container (possibly from multiple threads) and are
 * removed by consumers (possibly in other threads.)
 *
 * It provides a uniform API for all such containers, i.e. both queues
 * and sets, for code that wants to handle either type (e.g. for
 * queries.) The add() method will append to the end of a queue,
 * or, for sets, insert into the set, while remove() will remove from
 * the head of a queue, or, for sets, remove in whatever order the set
 * pool provides (using std::less by default.)
 *
 * Semantics include open() and close(). The close() method allows the
 * producer to tell consumers that it's done adding things. i.e. its
 * an end-of-stream indicator. (XXX Maybe this should be moved to the
 * LinkStreamValue base class ??)
 */
class ContainerValue
	: public LinkStreamValue
{
protected:
	ContainerValue(Type t) : LinkStreamValue(t) {}
	virtual void update() const;

public:
	ContainerValue(void) : LinkStreamValue(CONTAINER_VALUE) {}
	ContainerValue(const ValueSeq&);
	virtual ~ContainerValue() {}
	virtual void open(void) = 0;
	virtual void close(void) = 0;
	virtual bool is_open(void) = 0;

	virtual void add(const ValuePtr&) = 0;
	virtual ValuePtr remove(void) = 0;
	virtual size_t size(void) = 0;

	virtual void clear(void) = 0;
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<ContainerValue> ContainerValuePtr;
static inline ContainerValuePtr ContainerValueCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<ContainerValue>(a); }

/** @}*/
} // namespace opencog

#endif // _OPENCOG_CONTAINER_VALUE_H
