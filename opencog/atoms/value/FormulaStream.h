/*
 * opencog/atoms/value/FormulaStream.h
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

#ifndef _OPENCOG_FORMULA_STREAM_H
#define _OPENCOG_FORMULA_STREAM_H

#include <vector>
#include <opencog/atoms/value/StreamValue.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * FormulaStream will evaluate the stored Atom to obtain a fresh
 * FloatValue, every time it is queried for data.
 */
class FormulaStream
	: public StreamValue
{
protected:
	void init(void);
	virtual void update() const;
	HandleSeq _formula;
	AtomSpace* _as;

public:
	FormulaStream(const Handle&);
	FormulaStream(const HandleSeq&&);
	virtual ~FormulaStream() {}

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;

	/** Returns true if two values are equal. */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<FormulaStream> FormulaStreamPtr;
static inline FormulaStreamPtr FormulaStreamCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<FormulaStream>(a); }

template<typename ... Type>
static inline std::shared_ptr<FormulaStream> createFormulaStream(Type&&... args)
{
	return std::make_shared<FormulaStream>(std::forward<Type>(args)...);
}


/** @}*/
} // namespace opencog

#endif // _OPENCOG_FORMULA_STREAM_H
