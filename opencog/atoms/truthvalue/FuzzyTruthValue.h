/*
 * opencog/atoms/truthvalue/FuzzyTruthValue.h
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Guilherme Lamacie
 *            Murilo Queiroz <murilo@vettalabs.com>
 *            Welter Silva <welter@vettalabs.com>
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

#ifndef _OPENCOG_FUZZY_TRUTH_VALUE_H_
#define _OPENCOG_FUZZY_TRUTH_VALUE_H_

#include <opencog/atoms/truthvalue/TruthValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

// XXX FIXME Is this actually used anywehere for anything?
// Can we get rid of it?

class FuzzyTruthValue;
typedef std::shared_ptr<const FuzzyTruthValue> FuzzyTruthValuePtr;

//! A TruthValue that stores a mean and the number of observations
//! (strength and confidence)
class FuzzyTruthValue : public TruthValue
{
protected:
    enum {
        MEAN, /// Mean of the strength of the TV over all observations
        COUNT /// Total number of observations used to estimate the mean
    };

    void init(strength_t mean, count_t count);

    static count_t DEFAULT_K;
public:

    FuzzyTruthValue(strength_t mean, count_t count);
    FuzzyTruthValue(const TruthValue&);
    FuzzyTruthValue(FuzzyTruthValue const&);
    FuzzyTruthValue(const ValuePtr&);

    virtual bool operator==(const Value&) const;

    /// Heuristic to compute the count given the confidence (according
    /// to the PLN book)
    /// count =  confidence * k / (1 - confidence)
    /// where k is the look-ahead
    static count_t confidenceToCount(confidence_t);

    /// Heuristic to compute the confidence given the count (according
    /// to the PLN book)
    /// confidence = count / (count + k)
    /// where k is the look-ahead
    static confidence_t countToConfidence(count_t);

    std::string to_string(const std::string&) const;

    strength_t get_mean() const;
    count_t get_count() const;
    confidence_t get_confidence() const;

    /**
     * Truth value merge formula, as specified by PLN.
     *
     * Currently tv1.merge(tv2) works as follows:
     * the resulting TV is either tv1 or tv2, the result being the one
     * with the highest confidence.
     */
    TruthValuePtr merge(const TruthValuePtr&,
                        const MergeCtrl& mc=MergeCtrl()) const;

    static FuzzyTruthValuePtr createSTV(strength_t mean, count_t count)
    {
        return std::make_shared<FuzzyTruthValue>(mean, count);
    }
    static TruthValuePtr createTV(strength_t mean, count_t count)
    {
        return std::static_pointer_cast<const TruthValue>(createSTV(mean, count));
    }
    static TruthValuePtr createTV(const ValuePtr& pap)
    {
        return std::static_pointer_cast<const TruthValue>(
            std::make_shared<const FuzzyTruthValue>(pap));
    }
};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_FUZZY_TRUTH_VALUE_H_
