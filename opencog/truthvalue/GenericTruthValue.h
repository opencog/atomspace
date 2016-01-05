/*
 * GenericTruthValue.h
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

#ifndef _OPENCOG_GENERIC_TRUTH_VALUE_H
#define _OPENCOG_GENERIC_TRUTH_VALUE_H

#include <string>
#include <memory>

#include <opencog/util/exceptions.h>
#include <opencog/truthvalue/TruthValue.h>

namespace opencog
{

typedef float strength_t;
typedef float confidence_t;
typedef double count_t;
typedef double entropy_t;

class GenericTruthValue;
typedef std::shared_ptr<const GenericTruthValue> GenericTruthValuePtr;

class GenericTruthValue : public TruthValue
{
    // Truth values are immutable
    GenericTruthValue& operator=(const GenericTruthValue& rhs) {
        throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
    }

    public:
        GenericTruthValue(count_t, count_t,
                          strength_t, strength_t,
                          confidence_t, entropy_t);
        GenericTruthValue(GenericTruthValue const&);

        TruthValueType getType() const;
        strength_t getMean() const;
        count_t getCount() const;
        confidence_t getConfidence() const;

        count_t getPositiveEvidence() const;
        count_t getLogPositiveEvidence() const;

        count_t getTotalEvidence() const;
        count_t getLogTotalEvidence() const;

        strength_t getFrequency() const;
        strength_t getLogFrequency() const;

        strength_t getFuzzyStrength() const;
        strength_t getLogFuzzyStrength() const;

        confidence_t getLogConfidence() const;

        entropy_t getEntropy() const;

        TruthValuePtr merge(TruthValuePtr, const MergeCtrl& = MergeCtrl()) const;

        TruthValuePtr clone() const
        {
            return std::make_shared<GenericTruthValue>(*this);
        }

        TruthValue* rawclone() const
        {
            return new GenericTruthValue(*this);
        }

        bool operator==(const TruthValue&) const;
        std::string toString() const;

    protected:
        count_t positiveEvidence;

        // PLN count
        count_t totalEvidence;

        // Probabilistic strength
        strength_t frequency;

        // Fuzzy set membership strength
        strength_t fuzzyStrength;

        confidence_t confidence;

        entropy_t entropy;

};
} // namespace opencog

#endif // _OPENCOG_GENERIC_TRUTH_VALUE_H
