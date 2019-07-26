/*
 * opencog/atomspace/TruthValue.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Welter Silva <welter@vettalabs.com>
 *            Guilherme Lamacie
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

#include <sstream>

#include <float.h>
#include <math.h>
#include <stdio.h>

#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/FuzzyTruthValue.h>
#include <opencog/atoms/truthvalue/IndefiniteTruthValue.h>
#include <opencog/atoms/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/truthvalue/TruthValue.h>

namespace opencog {

const strength_t MAX_TRUTH  = 1.0;

std::string TruthValue::to_short_string(const std::string& indent) const
{
	return to_string(indent);
}

TruthValuePtr TruthValue::DEFAULT_TV()
{
	// True, but no confidence.
	static TruthValuePtr instance(std::make_shared<SimpleTruthValue>(MAX_TRUTH, 0.0));
	return instance;
}

TruthValuePtr TruthValue::TRUE_TV()
{
	// True, with maximum confidence.
	static TruthValuePtr instance(std::make_shared<SimpleTruthValue>(MAX_TRUTH, 1.0));
	return instance;
}

TruthValuePtr TruthValue::FALSE_TV()
{
	// False, with maximum confidence.
	static TruthValuePtr instance(std::make_shared<SimpleTruthValue>(0.0, 1.0));
	return instance;
}

TruthValuePtr TruthValue::TRIVIAL_TV()
{
	// False, with no confidence.
	static TruthValuePtr instance(std::make_shared<SimpleTruthValue>(0.0, 0.0));
	return instance;
}

bool TruthValue::isDefaultTV() const
{
	static TruthValuePtr dtv(DEFAULT_TV());
	if (dtv.get() == this) return true;
	if (get_type() == dtv->get_type() and
	    get_mean() == dtv->get_mean() and
	    get_confidence() == dtv->get_confidence())
	{
		return true;
	}
	return false;
}

/**
 * Return true if the TV value is one of the pre-defined TV values.
 */
bool TruthValue::isDefinedTV() const
{
	TruthValuePtr dtv = DEFAULT_TV();
	if (dtv.get() == this) return true;
	if (get_type() == dtv->get_type() and
	    get_mean() == dtv->get_mean() and
	    get_confidence() == dtv->get_confidence())
	{
		return true;
	}

	dtv = TRUE_TV();
	if (dtv.get() == this) return true;

	dtv = FALSE_TV();
	if (dtv.get() == this) return true;

	dtv = TRIVIAL_TV();
	if (dtv.get() == this) return true;

	dtv = TRUE_TV();
	if (get_type() == dtv->get_type() and
	    get_mean() == dtv->get_mean() and
	    get_confidence() == dtv->get_confidence())
	{
		return true;
	}

	dtv = FALSE_TV();
	if (get_type() == dtv->get_type() and
	    get_mean() == dtv->get_mean() and
	    get_confidence() == dtv->get_confidence())
	{
		return true;
	}

	dtv = TRIVIAL_TV();
	if (get_type() == dtv->get_type() and
	    get_mean() == dtv->get_mean() and
	    get_confidence() == dtv->get_confidence())
	{
		return true;
	}
	return false;
}

bool TruthValue::nearly_equal(double a, double b)
{
	if (a == b) return true;

// Anything smaller than this will fail BasicSaveUTest
#define ACCEPTABLE_ERROR (5.0 * DBL_EPSILON)
	double diff = fabs(b - a);
	if (a == 0.0 or b == 0.0 or diff < DBL_MIN)
		return diff < ACCEPTABLE_ERROR * DBL_MIN;

	double absa = fabs(a);
	double absb = fabs(b);
	return diff / fmin (absa+absb, DBL_MAX) < ACCEPTABLE_ERROR;
}

TruthValuePtr
TruthValue::higher_confidence_merge(const TruthValuePtr& other) const
{
	if (other->get_confidence() > get_confidence()) {
		return other;
	}
	return std::dynamic_pointer_cast<const TruthValue>(shared_from_this());
}

TruthValuePtr TruthValue::factory(Type t, const std::vector<double>& v)
{
	ValuePtr pap = createFloatValue(t,v);
	return factory(pap);
}

TruthValuePtr TruthValue::factory(const ValuePtr& pap)
{
	Type t = pap->get_type();
	if (SIMPLE_TRUTH_VALUE == t)
		return SimpleTruthValue::createTV(pap);
	if (COUNT_TRUTH_VALUE == t)
		return CountTruthValue::createTV(pap);
	if (FUZZY_TRUTH_VALUE == t)
		return FuzzyTruthValue::createTV(pap);
	if (INDEFINITE_TRUTH_VALUE == t)
		return IndefiniteTruthValue::createTV(pap);
	if (PROBABILISTIC_TRUTH_VALUE == t)
		return ProbabilisticTruthValue::createTV(pap);

	throw RuntimeException(TRACE_INFO,
		"Unknown TruthValue type %d", t);
	return nullptr;
}

std::string oc_to_string(TruthValuePtr tv, const std::string& indent)
{
	if (tv)
		return tv->to_string();
	return "none";
}

std::string oc_to_string(const TruthValueSeq& tvs, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << tvs.size();
	size_t i = 0;
	for (const TruthValuePtr& tv : tvs) {
		ss << std::endl << indent << "tv[" << i << "]: " << oc_to_string(tv);
		i++;
	}
	return ss.str();
}

} // ~namespace opencog
