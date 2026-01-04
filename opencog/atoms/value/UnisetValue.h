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
 * UnisetValues provide a thread-safe hoarding container for Values.
 * It is currently a mashup of several distinct ideas that could be
 * pulled apart; but for now, its easier to live with the mashup.
 * These are:
 *
 * Thread safety -- inherited from ContainerValue.
 *
 * Hoarding -- the ctor for the uniset allows the specification of
 *   an upstream source. This source is drained/emptied whenever
 *   the Uniset is accessed, moving values from upstream, to here.
 *   This allows the Uniset to apply operations for the entire set
 *   contents.
 *
 * Deduplication -- If the same Value is added twice to the Uniset,
 *   the second Value is ignored. The set can only contain one copy
 *   of a Value; that's what makes it "uni".
 *
 * Base class for RelationalValue -- The depduplication is done by
 *   using std::set; but atthis can provide the foundation for more
 *   general relational values.
 */
class UnisetValue
	: public ContainerValue
{
protected:
	// Provide thunk for ordering the Values in the set.
	struct ValueComp
	{
		UnisetValue* uv;
		ValueComp(UnisetValue* p) : uv(p) {}
		bool operator()(const ValuePtr& lhs, const ValuePtr& rhs) const
		{
			if (!lhs) return bool(rhs);   // null < non-null
			if (!rhs) return false;       // non-null >= null
			return uv->less(*lhs, *rhs);
		}
	};

	mutable concurrent_set<ValuePtr, ValueComp> _set;

	// Data source for hoarding.
	LinkValuePtr _source;
	void drain(void);
	void init_src(const ValuePtr&);

	UnisetValue(Type t) : ContainerValue(t), _set(ValueComp(this)), _source(nullptr) {}
	virtual void update() const override;

	// Default ordering inherited from Value::operator<()
	virtual bool less(const Value& lhs, const Value& rhs) const
	{ return lhs < rhs; }

public:
	UnisetValue(void) : ContainerValue(UNISET_VALUE), _set(ValueComp(this)), _source(nullptr) {}
	UnisetValue(const ValueSeq&);
	virtual ~UnisetValue() {}
	virtual void open(void);
	virtual void close(void);
	virtual bool is_closed(void) const;

	virtual void add(const ValuePtr&);
	virtual void add(ValuePtr&&);
	virtual ValuePtr remove(void);
	virtual ValuePtr peek(void) const;
	virtual size_t size(void) const;
	virtual void clear(void);
};

VALUE_PTR_DECL(UnisetValue);
CREATE_VALUE_DECL(UnisetValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_UNISET_VALUE_H
