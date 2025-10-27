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
	friend class TransposeColumn;
	friend class FlatStream;

protected:
	mutable std::vector<ValuePtr> _value;
	virtual void update() const {}

	std::string to_string(const std::string&, Type) const;
	LinkValue(Type t) : Value(t) {}
public:
	LinkValue(void)
		: Value(LINK_VALUE) {}

	LinkValue(const ValuePtr& vp)
		: Value(LINK_VALUE) { _value.push_back(vp); }

	LinkValue(const ValueSeq& vlist)
		: Value(LINK_VALUE), _value(vlist) {}

	LinkValue(ValueSeq&& vlist)
		: Value(LINK_VALUE), _value(std::move(vlist)) {}

protected:
	LinkValue(Type t, const ValueSeq& vlist)
		: Value(t), _value(vlist) {}

	LinkValue(Type t, ValueSeq&& vlist)
		: Value(t), _value(std::move(vlist)) {}

	LinkValue(Type t, const ValueSet& vset)
		: Value(t), _value(vset.begin(), vset.end()) {}

	LinkValue(Type t, const HandleSeq& hseq)
		: Value(t), _value(hseq.begin(), hseq.end()) {}

	LinkValue(Type t, const HandleSet& hset)
		: Value(t), _value(hset.begin(), hset.end()) {}

public:
	LinkValue(const ValueSet& vset)
		: Value(LINK_VALUE), _value(vset.begin(), vset.end()) {}

	LinkValue(const HandleSeq& hseq)
		: Value(LINK_VALUE), _value(hseq.begin(), hseq.end()) {}

	LinkValue(const HandleSet& hset)
		: Value(LINK_VALUE), _value(hset.begin(), hset.end()) {}

	virtual ~LinkValue() {}

	const ValueSeq& value() const { update(); return _value; }
	HandleSeq to_handle_seq(void) const;
	HandleSet to_handle_set(void) const;
	size_t size() const { return _value.size(); }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const
	{ return to_string(indent, _type); }
	virtual std::string to_short_string(const std::string& indent = "") const;

	/** Returns true if the two atoms are equal, else false.  */
	virtual bool operator==(const Value&) const;

	/** Optimized less-than comparison for LinkValue.
	 * Compares by type name first, then vector length, then individual values. */
	virtual bool operator<(const Value& other) const;
};

VALUE_PTR_DECL(LinkValue);
CREATE_VALUE_DECL(LinkValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_LINK_VALUE_H
