/*
 * opencog/atoms/truthvalue/FuzzyTruthValue.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Welter Silva <welter@vettalabs.com>
 *            Guilherme Lamacie

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
 *
 */

#include <math.h>
#include <typeinfo>

#include <opencog/util/platform.h>
#include <opencog/util/exceptions.h>

#include "FuzzyTruthValue.h"

using namespace opencog;

count_t FuzzyTruthValue::DEFAULT_K = 800.0;

FuzzyTruthValue::FuzzyTruthValue(strength_t m, count_t c)
	: TruthValue(FUZZY_TRUTH_VALUE)
{
    _value.resize(2);
    _value[MEAN] = m;
    _value[COUNT] = c;
}

FuzzyTruthValue::FuzzyTruthValue(const TruthValue& source)
	: TruthValue(FUZZY_TRUTH_VALUE)
{
    _value.resize(2);
    _value[MEAN] = source.get_mean();
    _value[COUNT] = source.get_count();
}

FuzzyTruthValue::FuzzyTruthValue(FuzzyTruthValue const& source)
	: TruthValue(FUZZY_TRUTH_VALUE)
{
    _value.resize(2);
    _value[MEAN] = source.get_mean();
    _value[COUNT] = source.get_count();
}

FuzzyTruthValue::FuzzyTruthValue(const ValuePtr& source)
    : TruthValue(FUZZY_TRUTH_VALUE)
{
    if (source->get_type() != FUZZY_TRUTH_VALUE)
        throw RuntimeException(TRACE_INFO,
            "Source must be a FuzzyTruthValue");

    FloatValuePtr fp(FloatValueCast(source));
    _value.resize(2);
    _value[MEAN] = fp->value()[MEAN];
    _value[COUNT] = fp->value()[COUNT];
}

strength_t FuzzyTruthValue::get_mean() const
{
    return _value[MEAN];
}

count_t FuzzyTruthValue::get_count() const
{
    return _value[COUNT];
}

confidence_t FuzzyTruthValue::get_confidence() const
{
    return countToConfidence(get_count());
}

// This is the merge formula appropriate for PLN.
TruthValuePtr FuzzyTruthValue::merge(const TruthValuePtr& other,
                                     const MergeCtrl& mc) const
{
    if (other->get_type() != SIMPLE_TRUTH_VALUE) {
        throw RuntimeException(TRACE_INFO,
           "Don't know how to merge %s into a FuzzyTruthValue",
           typeid(*other).name());
    }

    if (other->get_confidence() > get_confidence())
        return other;

    return std::static_pointer_cast<const TruthValue>(shared_from_this());
}

std::string FuzzyTruthValue::to_string(const std::string& indent) const
{
    char buf[1024];
    sprintf(buf, "(ftv %f %f)",
            static_cast<float>(get_mean()),
            static_cast<float>(get_confidence()));
    return buf;
}

bool FuzzyTruthValue::operator==(const Value& rhs) const
{
    const FuzzyTruthValue *ftv = dynamic_cast<const FuzzyTruthValue *>(&rhs);
    if (NULL == ftv) return false;

    if (not nearly_equal(get_mean(), ftv->get_mean())) return false;

// Converting from confidence to count and back again using single-precision
// float is a real accuracy killer.  In particular, 2/802 = 0.002494 but
// converting back gives 800*0.002494/(1.0-0.002494) = 2.000188 and so
// comparison tests can only be accurate to about 0.000188 or
// thereabouts.
#define FLOAT_ACCEPTABLE_COUNT_ERROR 0.0002

    if (FLOAT_ACCEPTABLE_COUNT_ERROR < fabs(1.0 - (ftv->get_count()/get_count()))) return false;
    return true;
}

count_t FuzzyTruthValue::confidenceToCount(confidence_t cf)
{
    // There are not quite 16 digits in double precision
    // not quite 7 in single-precision float
    cf = std::min(cf, 0.9999998);
    return static_cast<count_t>(DEFAULT_K * cf / (1.0 - cf));
}

confidence_t FuzzyTruthValue::countToConfidence(count_t cn)
{
    return static_cast<confidence_t>(cn / (cn + DEFAULT_K));
}
