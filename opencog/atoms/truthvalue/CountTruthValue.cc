/*
 * opencog/atoms/truthvalue/CountTruthValue.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Welter Silva <welter@vettalabs.com>
 *            Guilherme Lamacie
 *            Linas Vepstas <linasvepstas@gmail.com>
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

#include <opencog/util/platform.h>
#include <opencog/util/exceptions.h>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/value/ValueFactory.h>
#include "CountTruthValue.h"

using namespace opencog;

CountTruthValue::CountTruthValue(const std::vector<double>& v)
	: TruthValue(COUNT_TRUTH_VALUE)
{
    _value = v;
}

CountTruthValue::CountTruthValue(strength_t m, confidence_t n, count_t c)
	: TruthValue(COUNT_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = m;
    _value[CONFIDENCE] = n;
    _value[COUNT] = c;
}

CountTruthValue::CountTruthValue(const TruthValue& source)
	: TruthValue(COUNT_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = source.get_mean();
    _value[CONFIDENCE] = source.get_confidence();
    _value[COUNT] = source.get_count();
}

CountTruthValue::CountTruthValue(CountTruthValue const& source)
	: TruthValue(COUNT_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = source.get_mean();
    _value[CONFIDENCE] = source.get_confidence();
    _value[COUNT] = source.get_count();
}

CountTruthValue::CountTruthValue(const ValuePtr& source)
       : TruthValue(COUNT_TRUTH_VALUE)
{
    if (source->get_type() != COUNT_TRUTH_VALUE)
        throw RuntimeException(TRACE_INFO,
            "Source must be a CountTruthValue");

    FloatValuePtr fp(FloatValueCast(source));
    _value.resize(3);
    _value[MEAN] = fp->value()[MEAN];
    _value[CONFIDENCE] = fp->value()[CONFIDENCE];
    _value[COUNT] = fp->value()[COUNT];
}

strength_t CountTruthValue::get_mean() const
{
    return _value[MEAN];
}

count_t CountTruthValue::get_count() const
{
    return  _value[COUNT];
}

confidence_t CountTruthValue::get_confidence() const
{
    return _value[CONFIDENCE];
}

std::string CountTruthValue::to_string(const std::string& indent) const
{
    char buf[1024];
    sprintf(buf, "(ctv %g %g %g)",
            static_cast<float>(get_mean()),
            static_cast<double>(get_confidence()),
            static_cast<float>(get_count()));
    return buf;
}

bool CountTruthValue::operator==(const Value& rhs) const
{
    const CountTruthValue *ctv = dynamic_cast<const CountTruthValue *>(&rhs);
    if (NULL == ctv) return false;

    if (not nearly_equal(get_mean(), ctv->get_mean())) return false;
    if (not nearly_equal(get_confidence(), ctv->get_confidence())) return false;
    if (not nearly_equal(ctv->get_count(), get_count())) return false;

    return true;
}

// Note: this is NOT the merge formula used by PLN.  This is
// because the CountTruthValue usually stores an integer count,
// and a log-probability or entropy, instead of a confidence.
TruthValuePtr CountTruthValue::merge(const TruthValuePtr& other,
                                     const MergeCtrl& mc) const
{
    CountTruthValuePtr oc(CountTruthValueCast(other));

    // If other is a simple truth value, *and* its not the default TV,
    // then perhaps we should merge it in, as if it were a count truth
    // value with a count of 1?  In which case, we should add a merge
    // routine to SimpleTruthValue to do likewise... Anyway, for now,
    // just ignore this possible complication to the semantics.
    if (NULL == oc)
        return std::dynamic_pointer_cast<const TruthValue>(shared_from_this());

    // If both this and other are counts, then accumulate to get the
    // total count, and average together the strengths, using the
    // count as the relative weight.
    count_t cnt =  get_count() + oc->get_count();
    strength_t meeny = (get_mean() * get_count() +
                   oc->get_mean() * oc->get_count()) / cnt;

    // XXX This is not the correct way to handle confidence ...
    // The confidence will typically hold the log probability,
    // where the probability is the normalized count.  Thus
    // the right thing to do is probably to add the probabilities!?
    // However, this is not correct when the confidence is actually
    // holding the mutual information ... which is additive ...
    // Argh .. what to do?
    //    confidence = oc->confidence;

    return createTV(meeny, get_confidence(), cnt);
}

DEFINE_VALUE_FACTORY(COUNT_TRUTH_VALUE,
	createCountTruthValue, std::vector<double>)
