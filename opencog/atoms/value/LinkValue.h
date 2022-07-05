/*
 * opencog/atoms/value/LinkValue.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_LINK_VALUE_H
#define _OPENCOG_LINK_VALUE_H

#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * LinkValue holds an ordered vector of protoatoms.
 * (i.e. its a link, but for values)
 */
class LinkValue
	: public Value
{
protected:
	mutable std::vector<ValuePtr> _value;
	virtual void update() const {}

	LinkValue(Type t) : Value(t) {}
public:
	LinkValue(const ValueSeq& vlist)
		: Value(LINK_VALUE), _value(vlist) {}

	LinkValue(const ValueSet& vset)
		: Value(LINK_VALUE)
	{ for (const ValuePtr& v: vset) _value.emplace_back(v); }

	LinkValue(const HandleSeq& hseq)
		: Value(LINK_VALUE)
	{ for (const Handle& h: hseq) _value.emplace_back(h); }

	LinkValue(const HandleSet& hset)
		: Value(LINK_VALUE)
	{ for (const Handle& h: hset) _value.emplace_back(h); }

	virtual ~LinkValue() {}

	const std::vector<ValuePtr>& value() const { update(); return _value; }
	HandleSeq to_handle_seq(void) const;
	HandleSet to_handle_set(void) const;
	size_t size() const { return _value.size(); }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;
	virtual std::string to_short_string(const std::string& indent = "") const;

	/** Returns true if the two atoms are equal, else false.  */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<LinkValue> LinkValuePtr;
static inline LinkValuePtr LinkValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<LinkValue>(a); }
static inline const ValuePtr ValueCast(const LinkValuePtr& lv)
	{ return std::shared_ptr<Value>(lv, (Value*) lv.get()); }

template<typename ... Type>
static inline std::shared_ptr<LinkValue> createLinkValue(Type&&... args) {
	return std::make_shared<LinkValue>(std::forward<Type>(args)...);
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_LINK_VALUE_H
