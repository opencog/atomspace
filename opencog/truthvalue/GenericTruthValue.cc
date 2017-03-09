/*
 * GenericTruthValue.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Leung Man Hin <https://github.com/leungmanhin>
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
#include "GenericTruthValue.h"

using namespace opencog;

#define KKK 800.0
#define CVAL  0.2

GenericTruthValue::GenericTruthValue(count_t pe, count_t te,
                                     strength_t f, strength_t fs,
                                     confidence_t c, entropy_t e)
	: TruthValue(GENERIC_TRUTH_VALUE)
{
    _value.resize(6);
    _value[POSITIVE_EVIDENCE] = pe;
    _value[TOTAL_EVIDENCE] = te;
    _value[FREQUENCY] = f;
    _value[FUZZY_STRENGTH] = fs;
    _value[CONFIDENCE] = c;
    _value[ENTROPY] = e;
}

GenericTruthValue::GenericTruthValue(GenericTruthValue const& gtv)
	: TruthValue(GENERIC_TRUTH_VALUE)
{
    _value.resize(6);
    _value[POSITIVE_EVIDENCE] = gtv.getPositiveEvidence();
    _value[TOTAL_EVIDENCE] = gtv.getTotalEvidence();
    _value[FREQUENCY] = gtv.getFrequency();
    _value[FUZZY_STRENGTH] = gtv.getFuzzyStrength();
    _value[CONFIDENCE] = gtv.getConfidence();
    _value[ENTROPY] = gtv.getEntropy();
}

GenericTruthValue::GenericTruthValue(const ProtoAtomPtr& source)
       : TruthValue(GENERIC_TRUTH_VALUE)
{
    if (source->getType() != GENERIC_TRUTH_VALUE)
        throw RuntimeException(TRACE_INFO,
            "Source must be a GenericTruthValue");

    FloatValuePtr fp(FloatValueCast(source));
    _value.resize(2);
    _value[POSITIVE_EVIDENCE] = fp->value()[POSITIVE_EVIDENCE];
    _value[TOTAL_EVIDENCE] = fp->value()[TOTAL_EVIDENCE];
    _value[FREQUENCY] = fp->value()[FREQUENCY];
    _value[FUZZY_STRENGTH] = fp->value()[FUZZY_STRENGTH];
    _value[CONFIDENCE] = fp->value()[CONFIDENCE];
    _value[ENTROPY] = fp->value()[ENTROPY];
}

count_t GenericTruthValue::getCount() const
{
    return _value[FREQUENCY];
}

confidence_t GenericTruthValue::getConfidence() const
{
    return _value[CONFIDENCE];
}

strength_t GenericTruthValue::getMean() const
{
    return _value[TOTAL_EVIDENCE];
}

count_t GenericTruthValue::getPositiveEvidence() const
{
    return _value[POSITIVE_EVIDENCE];
}

count_t GenericTruthValue::getLogPositiveEvidence() const
{
    return log(_value[POSITIVE_EVIDENCE]);
}

count_t GenericTruthValue::getTotalEvidence() const
{
    return _value[TOTAL_EVIDENCE];
}

count_t GenericTruthValue::getLogTotalEvidence() const
{
    return log(_value[TOTAL_EVIDENCE]);
}

strength_t GenericTruthValue::getFrequency() const
{
    return _value[FREQUENCY];
}

strength_t GenericTruthValue::getLogFrequency() const
{
    return log(_value[FREQUENCY]);
}

strength_t GenericTruthValue::getFuzzyStrength() const
{
    return _value[FUZZY_STRENGTH];
}

strength_t GenericTruthValue::getLogFuzzyStrength() const
{
    return log(_value[FUZZY_STRENGTH]);
}

confidence_t GenericTruthValue::getLogConfidence() const
{
    return log(_value[CONFIDENCE]);
}

entropy_t GenericTruthValue::getEntropy() const
{
    return _value[ENTROPY];
}

TruthValuePtr GenericTruthValue::merge(const TruthValuePtr& tv, const MergeCtrl& mc)
{
    GenericTruthValuePtr gtv = std::dynamic_pointer_cast<const GenericTruthValue>(tv);
    if (NULL == gtv) return tv;
    auto other_te = gtv->getTotalEvidence();
    auto new_pe = getPositiveEvidence() + gtv->getPositiveEvidence();
    auto new_te = getTotalEvidence() + other_te
                  - std::min(getTotalEvidence(), other_te) * CVAL;
    auto new_f = (getFrequency() * getTotalEvidence() + gtv->getFrequency() * other_te)
                 / (getTotalEvidence() + other_te);
    auto new_fs = std::max(getFuzzyStrength(), gtv->getFuzzyStrength());
    auto new_c = new_te / (new_te + KKK);

    // XXX
    auto new_e = std::max(getEntropy(), gtv->getEntropy());

    return std::make_shared<GenericTruthValue>(new_pe, new_te, new_f, new_fs,
                                               new_c, new_e);
}


bool GenericTruthValue::operator==(const ProtoAtom& rhs) const
{
    const GenericTruthValue *gtv = dynamic_cast<const GenericTruthValue *>(&rhs);
    if (NULL == gtv) return false;

#define FLOAT_ACCEPTABLE_ERROR 0.000001
    if (FLOAT_ACCEPTABLE_ERROR < fabs(getFrequency() - gtv->getFrequency()))
        return false;
    if (FLOAT_ACCEPTABLE_ERROR < fabs(getFuzzyStrength() - gtv->getFuzzyStrength()))
        return false;
    if (FLOAT_ACCEPTABLE_ERROR < fabs(getConfidence() - gtv->getConfidence()))
        return false;

#define DOUBLE_ACCEPTABLE_ERROR 1.0e-14
    if (DOUBLE_ACCEPTABLE_ERROR < fabs(1.0 - (gtv->getPositiveEvidence()/getPositiveEvidence())))
        return false;
    if (DOUBLE_ACCEPTABLE_ERROR < fabs(1.0 - (gtv->getTotalEvidence()/getTotalEvidence())))
        return false;
    if (DOUBLE_ACCEPTABLE_ERROR < fabs(1.0 - (gtv->getEntropy()/getEntropy())))
        return false;

    return true;
}

std::string GenericTruthValue::toString(const std::string& indent) const
{
    char buf[1024];
    sprintf(buf, "(gtv %f %f %f %f %f %f)",
            static_cast<double>(getPositiveEvidence()),
            static_cast<double>(getTotalEvidence()),
            static_cast<float>(getFrequency()),
            static_cast<float>(getFuzzyStrength()),
            static_cast<float>(getConfidence()),
            static_cast<double>(getEntropy()));
    return buf;
}
