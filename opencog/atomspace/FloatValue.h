/*
 * opencog/atomspace/FloatValue.h
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

#ifndef _OPENCOG_FLOAT_VALUE_H
#define _OPENCOG_FLOAT_VALUE_H

#include <opencog/atomspace/ProtoAtom.h>
#include <opencog/atomspace/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * FloatValues are the base class for the Atom shared pointer.
 */
class FloatValue
	: public ProtoAtom
{
private:
	double _value;

public:
	FloatValue(double v) : ProtoAtom(FLOAT_VALUE), _value(v) {}

	virtual ~FloatValue() {}

	/** Returns a string representation of the node.
	 *
	 * @return A string representation of the node.
	 * cannot be const, because observing the TV and AV requires a lock.
	 */
	virtual std::string toString(std::string indent);
	virtual std::string toShortString(std::string indent)
	{ return toString(indent); }

	/** Returns whether two atoms are equal.
	 *
	 * @return true if the atoms are equal, false otherwise.
	 */
	virtual bool operator==(const ProtoAtom&) const;

	/** Returns whether two atoms are different.
	 *
	 * @return true if the atoms are different, false otherwise.
	 */
	bool operator!=(const ProtoAtom& other) const
	{  return not operator==(other); }
};

typedef std::shared_ptr<FloatValue> FloatValuePtr;
static inline FloatValuePtr FloatValueCast(const Handle& h)
	{ ProtoAtomPtr a(h); return std::dynamic_pointer_cast<FloatValue>(a); }
static inline FloatValuePtr FloatValueCast(ProtoAtomPtr& a)
	{ return std::dynamic_pointer_cast<FloatValue>(a); }

// XXX temporary hack ...
#define createFloatValue std::make_shared<FloatValue>


/** @}*/
} // namespace opencog

#endif // _OPENCOG_FLOAT_VALUE_H
