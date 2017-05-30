/*
 * opencog/truthvalue/ProbabilisticTruthValue.cc
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

#include <math.h>

#include <opencog/util/platform.h>
#include <opencog/util/exceptions.h>

#include <opencog/atoms/base/atom_types.h>
#include "ProbabilisticTruthValue.h"

using namespace opencog;

ProbabilisticTruthValue::ProbabilisticTruthValue(strength_t m, confidence_t n, count_t c)
	: TruthValue(PROBABILISTIC_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = m;
    _value[CONFIDENCE] = n;
    _value[COUNT] = c;
}

ProbabilisticTruthValue::ProbabilisticTruthValue(const TruthValue& source)
	: TruthValue(PROBABILISTIC_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = source.getMean();
    _value[CONFIDENCE] = source.getConfidence();
    _value[COUNT] = source.getCount();
}

ProbabilisticTruthValue::ProbabilisticTruthValue(ProbabilisticTruthValue const& source)
	: TruthValue(PROBABILISTIC_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = source.getMean();
    _value[CONFIDENCE] = source.getConfidence();
    _value[COUNT] = source.getCount();
}

ProbabilisticTruthValue::ProbabilisticTruthValue(const ProtoAtomPtr& source)
       : TruthValue(PROBABILISTIC_TRUTH_VALUE)
{
    if (source->getType() != PROBABILISTIC_TRUTH_VALUE)
        throw RuntimeException(TRACE_INFO,
            "Source must be a ProbabilisticTruthValue");

    FloatValuePtr fp(FloatValueCast(source));
    _value.resize(3);
    _value[MEAN] = fp->value()[MEAN];
    _value[CONFIDENCE] = fp->value()[CONFIDENCE];
    _value[COUNT] = fp->value()[COUNT];
}

strength_t ProbabilisticTruthValue::getMean() const
{
    return _value[MEAN];
}

count_t ProbabilisticTruthValue::getCount() const
{
    return  _value[COUNT];
}

confidence_t ProbabilisticTruthValue::getConfidence() const
{
    return _value[CONFIDENCE];
}

std::string ProbabilisticTruthValue::toString(const std::string& indent) const
{
    char buf[1024];
    sprintf(buf, "(ctv %f %f %f)",
            static_cast<float>(getMean()),
            static_cast<float>(getCount()),
            static_cast<double>(getConfidence()));
    return buf;
}

bool ProbabilisticTruthValue::operator==(const ProtoAtom& rhs) const
{
    const ProbabilisticTruthValue *ctv = dynamic_cast<const ProbabilisticTruthValue *>(&rhs);
    if (NULL == ctv) return false;

#define FLOAT_ACCEPTABLE_ERROR 0.000001
    if (FLOAT_ACCEPTABLE_ERROR < fabs(getMean() - ctv->getMean())) return false;
    if (FLOAT_ACCEPTABLE_ERROR < fabs(getConfidence() - ctv->getConfidence())) return false;
#define DOUBLE_ACCEPTABLE_ERROR 1.0e-14
    if (DOUBLE_ACCEPTABLE_ERROR < fabs(1.0 - (ctv->getCount()/getCount()))) return false;

    return true;
}

// Note: this is NOT the merge formula used by PLN.  This is
// because the ProbabilisticTruthValue usally stores an integer count,
// and a log-probability or entropy, instead of a confidence.
TruthValuePtr ProbabilisticTruthValue::merge(const TruthValuePtr& other,
                                     const MergeCtrl& mc) const
{
    ProbabilisticTruthValuePtr oc =
        std::dynamic_pointer_cast<const ProbabilisticTruthValue>(other);

    // If other is a simple truth value, *and* its not the default TV,
    // then perhaps we should merge it in, as if it were a count truth
    // value with a count of 1?  In which case, we should add a merge
    // routine to SimpleTruthValue to do likewise... Anyway, for now,
    // just ignore this possible complication to the semantics.
    if (nullptr == oc)
        return std::static_pointer_cast<const TruthValue>(shared_from_this());

    // If both this and other are counts, then accumulate to get the
    // total count, and average together the strengths, using the
    // count as the relative weight.
    count_t cnt =  getCount() + oc->getCount();
    strength_t meeny = (getMean() * getCount() +
                   oc->getMean() * oc->getCount()) / cnt;

    // XXX This is not the correct way to handle confidence ...
    // The confidence will typically hold the log probability,
    // where the probability is the normalized count.  Thus
    // the right thing to do is probably to add the probabilities!?
    // However, this is not correct when the confidence is actually
    // holding the mutual information ... which is additive ...
    // Argh .. what to do?
    //    confidence = oc->confidence;

    return createTV(meeny, getConfidence(), cnt);
}
