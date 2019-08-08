/*
 * opencog/atoms/value/ProducerConsumerValue.h
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
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

#ifndef _OPENCOG_PRODUCER_CONSUMER_CONTROL_VALUE_H
#define _OPENCOG_PRODUCER_CONSUMER_CONTROL_VALUE_H

#include <string>
#include <vector>
#include <queue>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

class ProducerConsumerControl;

typedef void (Consumer)(Handle);
typedef std::queue<Handle> HandleQueue;
typedef std::shared_ptr<ProducerConsumerControl> ProducerConsumerControlPtr;

/**
 * ProducerConsumerControlValues is a structure pass atoms from producer to consumer.
 */
class ProducerConsumerControl
{
protected:
	HandleQueue _queue;
	void (*_consume)(Handle);

public:

	virtual ~ProducerConsumerControl() {}

	virtual void produce(const Handle& h);

	virtual void subscribe(Consumer);

	virtual void finished();
};

class ProducerConsumerValue
	: public Value
{
protected:
	std::string _name;
	ProducerConsumerControlPtr _control;

public:

	static const Handle CONTROL_KEY;

	ProducerConsumerValue(const std::string& name)
		: Value(PRODUCER_CONSUMER_VALUE),
		_name(name),
		_control(new ProducerConsumerControl()) { }

	virtual ~ProducerConsumerValue() {}

	ProducerConsumerControlPtr get_control() const { return _control; }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;

	/** Returns true if the two atoms are equal.  */
	virtual bool operator==(const Value&) const;

};


typedef std::shared_ptr<const ProducerConsumerValue> ProducerConsumerValuePtr;
static inline ProducerConsumerValuePtr ProducerConsumerValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<const ProducerConsumerValue>(a); }

template<typename ... Type>
static inline std::shared_ptr<ProducerConsumerValue> createProducerConsumerValue(Type&&... args) {
	return std::make_shared<ProducerConsumerValue>(std::forward<Type>(args)...);
}


/** @}*/
} // namespace opencog

#endif // _OPENCOG_PRODUCER_CONSUMER_CONTROL_VALUE_H
