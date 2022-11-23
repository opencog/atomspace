/*
 * opencog/atoms/truthvalue/FutureTruthValue.h
 *
 * Copyright (C) 2020,2022 Linas Vepstas
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

#ifndef _OPENCOG_FUTURE_TRUTH_VALUE_H_
#define _OPENCOG_FUTURE_TRUTH_VALUE_H_

#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

//! A TruthValue that recomputes the TV from a stored predicate.
class FutureTruthValue : public SimpleTruthValue
{
protected:
	virtual void update(void) const;
	Handle _future;
	AtomSpace* _as;

public:
	FutureTruthValue(const Handle&);
	FutureTruthValue(const HandleSeq&&);
	virtual ~FutureTruthValue();

	virtual bool operator==(const Value& rhs) const;

	std::string to_string(const std::string&) const;

	/// Update the values. The get_confidence() method is NOT
	/// overloaded because only one update is needed/desired.
	virtual strength_t get_mean() const;
};

typedef std::shared_ptr<const FutureTruthValue> FutureTruthValuePtr;
static inline FutureTruthValuePtr FutureTruthValueCast(ValuePtr& a)
	{ return std::dynamic_pointer_cast<FutureTruthValue>(a); }

template<typename ... Type>
static inline std::shared_ptr<FutureTruthValue> createFutureTruthValue(Type&&... args)
{
   return std::make_shared<FutureTruthValue>(std::forward<Type>(args)...);
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_FUTURE_TRUTH_VALUE_H_
