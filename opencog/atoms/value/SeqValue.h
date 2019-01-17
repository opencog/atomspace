/*
 * opencog/atoms/value/SeqValue.h
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

#ifndef _OPENCOG_SEQ_VALUE_H
#define _OPENCOG_SEQ_VALUE_H

#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * SeqValue holds an ordered vector of protoatoms.
 * (i.e. its a link, but for values)
 */
class SeqValue
	: public Value
{
protected:
	std::vector<ValuePtr> _value;

public:
	SeqValue(const std::vector<ValuePtr>& v)
		: Value(SEQ_VALUE), _value(v) {}

	virtual ~SeqValue() {}

	const std::vector<ValuePtr>& value() const { return _value; }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent) const;

	/** Returns true if the two atoms are equal, else false.  */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<SeqValue> SeqValuePtr;
static inline SeqValuePtr SeqValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<SeqValue>(a); }

template<typename ... Type>
static inline std::shared_ptr<SeqValue> createSeqValue(Type&&... args) {
	return std::make_shared<SeqValue>(std::forward<Type>(args)...);
}


/** @}*/
} // namespace opencog

#endif // _OPENCOG_SEQ_VALUE_H
