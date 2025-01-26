/*
 * opencog/atoms/value/UnisetValue.h
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

#ifndef _OPENCOG_UNISET_VALUE_H
#define _OPENCOG_UNISET_VALUE_H

#include <opencog/util/concurrent_set.h>
#include <opencog/atoms/value/ContainerValue.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * UnisetValues provide a thread-safe uniset of Values. They are
 * meant to be used for producer-consumer APIs, where the produced
 * values are added, possibly in different threads from which they
 * are removed.  This is a uniset, in that elements are deduplicated
 * so that the set contains only one copy of a given element.
 */
class UnisetValue
	: public ContainerValue, protected concurrent_set<ValuePtr>
{
protected:
	UnisetValue(Type t) : ContainerValue(t) {}
	virtual void update() const;

public:
	UnisetValue(void) : ContainerValue(UNISET_VALUE) {}
	UnisetValue(const ValueSeq&);
	virtual ~UnisetValue() {}
	virtual void open(void);
	virtual void close(void);
	virtual bool is_closed(void) const;

	virtual void add(const ValuePtr&);
	virtual void add(ValuePtr&&);
	virtual ValuePtr remove(void);
	virtual size_t size(void) const;
	virtual void clear(void);

	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<UnisetValue> UnisetValuePtr;
static inline UnisetValuePtr UnisetValueCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<UnisetValue>(a); }

template<typename ... Type>
static inline std::shared_ptr<UnisetValue> createUnisetValue(Type&&... args) {
   return std::make_shared<UnisetValue>(std::forward<Type>(args)...);
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_UNISET_VALUE_H
