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

#include "GenericTruthValue.h"

using namespace opencog;

GenericTruthValue::GenericTruthValue(count_t pe, count_t te,
                                     strength_t f, strength_t fs,
                                     confidence_t c, entropy_t e)
{
    positiveEvidence = pe;
    totalEvidence = te;
    frequency = f;
    fuzzyStrength = fs;
    confidence = c;
    entropy = e;
}

GenericTruthValue::GenericTruthValue(GenericTruthValue const& gtv)
{
    positiveEvidence = gtv.positiveEvidence;
    totalEvidence = gtv.totalEvidence;
    frequency = gtv.frequency;
    fuzzyStrength = gtv.fuzzyStrength;
    confidence = gtv.confidence;
    entropy = gtv.entropy;
}

count_t GenericTruthValue::getPositiveEvidence() const
{
    return positiveEvidence;
}

count_t GenericTruthValue::getTotalEvidence() const
{
    return totalEvidence;
}

strength_t GenericTruthValue::getfrequency() const
{
    return frequency;
}

strength_t GenericTruthValue::getFuzzyStrength() const
{
    return fuzzyStrength;
}

confidence_t GenericTruthValue::getConfidence() const
{
    return confidence;
}

entropy_t GenericTruthValue::getEntropy() const
{
    return entropy;
}

bool GenericTruthValue::operator==(const GenericTruthValue& rhs) const
{
    if (NULL == rhs) return false;

#define FLOAT_ACCEPTABLE_ERROR 0.000001
    if (FLOAT_ACCEPTABLE_ERROR < fabs(frequency - rhs.frequency))
        return false;
    if (FLOAT_ACCEPTABLE_ERROR < fabs(fuzzyStrength - rhs.fuzzyStrength))
        return false;
    if (FLOAT_ACCEPTABLE_ERROR < fabs(confidence - rhs.confidence))
        return false;

#define DOUBLE_ACCEPTABLE_ERROR 1.0e-14
    if (DOUBLE_ACCEPTABLE_ERROR < fabs(1.0 - (rhs.positiveEvidence/positiveEvidence)))
        return false;
    if (DOUBLE_ACCEPTABLE_ERROR < fabs(1.0 - (rhs.totalEvidence/totalEvidence)))
        return false;
    if (DOUBLE_ACCEPTABLE_ERROR < fabs(1.0 - (rhs.entropy/entropy)))
        return false;

    return true;
}

std::string GenericTruthValue::toString() const
{
    char buf[1024];
    sprintf(buf, "(gtv %f %f %f %f %f %f)",
            static_cast<double>(getPositiveEvidence()),
            static_cast<double>(getTotalEvidence()),
            static_cast<float>(getfrequency()),
            static_cast<float>(getFuzzyStrength()),
            static_cast<float>(getConfidence()),
            static_cast<double>(getEntropy()));
    return buf;
}
