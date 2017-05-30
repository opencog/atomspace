/*
 * opencog/atoms/base/Valuation.h
 *
 * Copyright (C) 2015,2017 Linas Vepstas
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

#ifndef _OPENCOG_VALUATION_H
#define _OPENCOG_VALUATION_H

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/ProtoAtom.h>
#include <opencog/atoms/base/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * A Valuation resembles an EvaluationLink, except that its a bit
 * more strictly defined, and is intended to be used for associating
 * Values with Atoms.
 */
class Valuation
	: public ProtoAtom
{
protected:
	Handle _key;
	Handle _atom;
	ProtoAtomPtr _value;  // XXX TODO should this be  vector?

public:
	Valuation(const Handle& k, const Handle& a, const ProtoAtomPtr& v)
		: ProtoAtom(VALUATION), _key(k), _atom(a), _value(v) {}

	virtual ~Valuation() {}

	const Handle& key() { return _key; }
	const Handle& atom() { return _atom; }
	ProtoAtomPtr& value() { return _value; }

	void setValue(const ProtoAtomPtr&);

	/** Returns a string representation of the value.  */
	virtual std::string toString(const std::string& indent) const;

	/** Returns true if the two atoms are equal, else false.  */
	virtual bool operator==(const ProtoAtom&) const;
};

typedef std::shared_ptr<Valuation> ValuationPtr;
static inline ValuationPtr ValuationCast(const ProtoAtomPtr& a)
	{ return std::dynamic_pointer_cast<Valuation>(a); }

// XXX temporary hack ...
#define createValuation std::make_shared<Valuation>


/** @}*/
} // namespace opencog

#endif // _OPENCOG_VALUATION_H
