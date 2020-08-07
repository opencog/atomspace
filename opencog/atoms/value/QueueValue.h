/*
 * opencog/atoms/value/QueueValue.h
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

#ifndef _OPENCOG_QUEUE_VALUE_H
#define _OPENCOG_QUEUE_VALUE_H

#include <opencog/util/concurrent_queue.h>
#include <opencog/atoms/value/LinkStreamValue.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * QueueValues provide a thread-safe FIFO queue of Values. They are
 * meant to be used for producer-consumer APIs, where the produced
 * values are to be handled in sequential order, in a different thread.
 */
class QueueValue
	: public LinkStreamValue, public concurrent_queue<ValuePtr>
{
protected:
	QueueValue(Type t) : LinkStreamValue(t) {}
	virtual void update() const;

public:
	QueueValue(void) : LinkStreamValue(QUEUE_VALUE) {}
	QueueValue(const ValueSeq&);
	virtual ~QueueValue() {}
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<QueueValue> QueueValuePtr;
static inline QueueValuePtr QueueValueCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<QueueValue>(a); }

template<typename ... Type>
static inline std::shared_ptr<QueueValue> createQueueValue(Type&&... args) {
   return std::make_shared<QueueValue>(std::forward<Type>(args)...);
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_QUEUE_VALUE_H
