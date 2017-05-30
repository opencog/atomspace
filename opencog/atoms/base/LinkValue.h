/*
 * opencog/atoms/base/LinkValue.h
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
#include <opencog/atoms/base/ProtoAtom.h>
#include <opencog/atoms/base/atom_types.h>

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
	: public ProtoAtom
{
protected:
	std::vector<ProtoAtomPtr> _value;

public:
	LinkValue(const std::vector<ProtoAtomPtr>& v)
		: ProtoAtom(LINK_VALUE), _value(v) {}

	virtual ~LinkValue() {}

	const std::vector<ProtoAtomPtr>& value() const { return _value; }

	/** Returns a string representation of the value.  */
	virtual std::string toString(const std::string& indent) const;

	/** Returns true if the two atoms are equal, else false.  */
	virtual bool operator==(const ProtoAtom&) const;
};

typedef std::shared_ptr<LinkValue> LinkValuePtr;
static inline LinkValuePtr LinkValueCast(const ProtoAtomPtr& a)
	{ return std::dynamic_pointer_cast<LinkValue>(a); }

// XXX temporary hack ...
#define createLinkValue std::make_shared<LinkValue>


/** @}*/
} // namespace opencog

#endif // _OPENCOG_LINK_VALUE_H
