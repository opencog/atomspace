/*
 * opencog/truthvalue/CountTruthValue.h
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

#ifndef _OPENCOG_COUNT_TRUTH_VALUE_H_
#define _OPENCOG_COUNT_TRUTH_VALUE_H_

#include <opencog/truthvalue/TruthValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class CountTruthValue;
typedef std::shared_ptr<const CountTruthValue> CountTruthValuePtr;

//! a TruthValue that stores a mean, a confidence and the number of observations
class CountTruthValue : public TruthValue
{
protected:
    enum {
        MEAN, /// Mean of the strength of the TV over all observations.
        CONFIDENCE, /// Estimate of confidence of the observation.
        COUNT /// Raw count
    };

public:

    CountTruthValue(strength_t, confidence_t, count_t);
    CountTruthValue(const TruthValue&);
    CountTruthValue(CountTruthValue const&);
    CountTruthValue(const ProtoAtomPtr&);

    virtual bool operator==(const ProtoAtom& rhs) const;

    virtual std::string toString(const std::string& = "") const;

    strength_t getMean() const;
    count_t getCount() const;
    confidence_t getConfidence() const;

    virtual TruthValuePtr merge(const TruthValuePtr&,
                                const MergeCtrl& mc=MergeCtrl()) const;

    static TruthValuePtr createTV(strength_t s, confidence_t f, count_t c)
    {
        return std::static_pointer_cast<const TruthValue>(
            std::make_shared<const CountTruthValue>(s, f, c));
    }
    static TruthValuePtr createTV(const ProtoAtomPtr& pap)
    {
        return std::static_pointer_cast<const TruthValue>(
            std::make_shared<const CountTruthValue>(pap));
    }

    TruthValuePtr clone() const
    {
        return std::make_shared<CountTruthValue>(*this);
    }
    TruthValue* rawclone() const
    {
        return new CountTruthValue(*this);
    }
};

static inline CountTruthValuePtr CountTruthValueCast(const TruthValuePtr& tv)
    { return std::dynamic_pointer_cast<const CountTruthValue>(tv); }


/** @}*/
} // namespace opencog

#endif // _OPENCOG_COUNT_TRUTH_VALUE_H_
