/*
 * opencog/truthvalue/DistributionalValue.h
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

#ifndef _OPENCOG_DISTRIBUTIONAL_VALUE_H
#define _OPENCOG_DISTRIBUTIONAL_VALUE_H

#include <memory>
#include <string>
#include <vector>
#include <limits>

#include <opencog/util/exceptions.h>
#include <opencog/atoms/proto/ProtoAtom.h>
#include <opencog/atoms/base/Handle.h>

#include <opencog/truthvalue/ConditionalDV.h>

/** \addtogroup grp_atomspace
 *	@{
 */

namespace opencog
{

typedef double strength_t;
typedef double confidence_t;
typedef double count_t;

class DistributionalValue;

typedef std::vector<double> DVec;

typedef std::vector<double> Interval;

typedef std::vector<Interval> DVKey;

typedef std::vector<DVKey> DVKeySeq;

typedef Counter<DVKey, double> DVCounter;

typedef std::shared_ptr<const DistributionalValue> DistributionalValuePtr;

class AtomSpace;

class DistributionalValue
	: public ProtoAtom
{

	friend class ConditionalDV;

	DVCounter _value;

	// Disallow assignment -- truth values are immutable!
	DistributionalValue& operator=(const DistributionalValue& rhs) {
		throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
	}


public:
	static count_t DEFAULT_K;

	DistributionalValue();
	DistributionalValue(DVCounter);
	DistributionalValue(double,double);

	const DVCounter& value() const { return _value; }

	static DistributionalValuePtr UniformDistributionalValue(DVKey,int);
	static DistributionalValuePtr UniformDistributionalValue(DVKeySeq,int);
	static DistributionalValuePtr TRUE_TV();
	static DistributionalValuePtr FALSE_TV();
	static DistributionalValuePtr DEFAULT_TV();
	static DistributionalValuePtr createDV(double,double);
	static DistributionalValuePtr createDV(DVCounter);

	static double to_conf(int c);
	static int to_count(double);

	bool is_uniform() const;

	std::vector<double> get_mode() const;
	std::vector<double> get_mean() const;
	std::vector<double> get_var() const;

	double get_fstord_mean() const;
	DVec middle_of_interval(DVKey) const;

	double get_mode_for(double) const;
	double get_mean_for(double) const;
	double get_var_for(double) const;

	DistributionalValuePtr add_evidence(DVKey) const;
	DistributionalValuePtr merge(DistributionalValuePtr) const;
	DistributionalValuePtr negate() const;

	double min_count() const;
	double max_count() const;

	double total_count() const;
	double get_confidence() const;

	static double key_contained(DVKey k1,DVKey k2);

	bool has_key(DVKey) const;
	DVKeySeq get_keys() const;
	double get_count(DVKey) const;
	double get_contained_count(DVKey) const;
	double get_mean(DVKey) const;
	double get_contained_mean(DVKey) const;
	double get_mode(DVKey) const;
	double get_var(DVKey) const;

	virtual bool operator==(const ProtoAtom& rhs) const;

	virtual bool operator<(const ProtoAtom&) const;

	using ProtoAtom::to_string;
	std::string to_string(const std::string&) const;
};

static inline DistributionalValuePtr DistributionalValueCast(const ProtoAtomPtr& pa)
	{ return std::dynamic_pointer_cast<const DistributionalValue>(pa); }

static inline ProtoAtomPtr ProtoAtomCast(const DistributionalValuePtr& dv)
{
	// This should have worked!?
	// return std::const_pointer_cast<ProtoAtom>(tv);

	// This, too, should have worked!?
	// return std::shared_ptr<ProtoAtom>(tv, const_cast<ProtoAtom*>(tv.get()));

	// This works...
	return std::shared_ptr<ProtoAtom>(dv, (ProtoAtom*) dv.get());
}

std::string oc_to_string(const DistributionalValue& hc);

} // namespace opencog

/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
