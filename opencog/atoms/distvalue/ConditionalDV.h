/*
 * opencog/truthvalue/ConditionalGDTV.h
 *
 * Copyright (C) 2018 SingularityNet
 * All Rights Reserved
 *
 * Written by Roman Treutlein
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

#ifndef _OPENCOG_CONDITIONAL_GDTV_H
#define _OPENCOG_CONDITIONAL_GDTV_H

#include <memory>
#include <string>
#include <vector>
#include <limits>

#include <opencog/util/exceptions.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/distvalue/DistributionalValue.h>

/** \addtogroup grp_atomspace
 *	@{
 */

namespace opencog
{

class DistributionalValue;
typedef std::vector<double> Interval;
typedef std::vector<Interval> DVKey;
typedef std::vector<DVKey> DVKeySeq;
typedef Counter<DVKey, double> DVCounter;
typedef std::shared_ptr<const DistributionalValue> DistributionalValuePtr;

class ConditionalDV;
typedef std::shared_ptr<const ConditionalDV> ConditionalDVPtr;

typedef std::map<DVKey,DVCounter> CDVrep;

class ConditionalDV
	: public Value
{
	CDVrep _value;

	// Disallow assignment -- truth values are immutable!
	ConditionalDV& operator=(const ConditionalDV& rhs) {
		throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
	}

public:
	ConditionalDV();
	ConditionalDV(CDVrep);
	ConditionalDV(DVKeySeq,std::vector<DistributionalValuePtr>);

	const CDVrep& value() const { return _value; }

	static ConditionalDVPtr createCDV();
	static ConditionalDVPtr createCDV(CDVrep);
	static ConditionalDVPtr createCDV(DVKeySeq,std::vector<DistributionalValuePtr>);

	ConditionalDVPtr merge(ConditionalDVPtr) const;

	DVKeySeq get_conditions() const;
	std::vector<DistributionalValuePtr> get_unconditionals() const;

	DVCounter get_unconditionalP(DVKey) const;
	DistributionalValuePtr get_unconditional(DVKey) const;
	DistributionalValuePtr get_unconditional(DistributionalValuePtr) const;

	DistributionalValuePtr get_joint_probability(DistributionalValuePtr) const;

	//Consequent-Disjuction-Elimination
	ConditionalDVPtr CDE(ConditionalDVPtr) const;

	double total_count() const;
	double avg_count() const;

	virtual bool operator==(const Value& rhs) const;

	using Value::to_string;
	std::string to_string(const std::string&) const;

};

static inline ConditionalDVPtr ConditionalDVCast(const ValuePtr& pa)
	{ return std::dynamic_pointer_cast<const ConditionalDV>(pa); }

static inline ValuePtr ValueCast(const ConditionalDVPtr& cdv)
{
	// This should have worked!?
	// return std::const_pointer_cast<Value>(tv);

	// This, too, should have worked!?
	// return std::shared_ptr<Value>(tv, const_cast<Value*>(tv.get()));

	// This works...
	return std::shared_ptr<Value>(cdv, (Value*) cdv.get());
}
} // namespace opencog

/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
