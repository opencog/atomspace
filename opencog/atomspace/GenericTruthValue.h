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

namespace opencog
{

typedef float strength_t;
typedef float confidence_t;
typedef double count_t;
typedef double entropy_t;

class GenericTruthValue;
typedef std::shared_ptr<GenericTruthValue> GenericTruthValuePtr;

class GenericTruthValue
        : public std::enable_shared_from_this<GenericTruthValue>
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

        count_t getPositiveEvidence() const;
        count_t getTotalEvidence() const;
        strength_t getFrequency() const;
        strength_t getFuzzyStrength() const;
        confidence_t getConfidence() const;
        entropy_t getEntropy() const;

        GenericTruthValuePtr merge(GenericTruthValuePtr) const;

        GenericTruthValuePtr clone() const
        {
            return std::make_shared<GenericTruthValue>(*this);
        }

        GenericTruthValue* rawclone() const
        {
            return new GenericTruthValue(*this);
        }

        virtual bool operator==(const GenericTruthValue& rhs) const;
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
