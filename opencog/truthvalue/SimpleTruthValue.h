/*
 * opencog/truthvalue/SimpleTruthValue.h
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

#ifndef _OPENCOG_SIMPLE_TRUTH_VALUE_H_
#define _OPENCOG_SIMPLE_TRUTH_VALUE_H_

#include <opencog/truthvalue/TruthValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class SimpleTruthValue;
typedef std::shared_ptr<SimpleTruthValue> SimpleTruthValuePtr;

//! a TruthValue that stores a strength and confidence.
class SimpleTruthValue : public TruthValue
{
protected:

    /// Mean of the strength of the TV over all observations.
    strength_t _mean;

    /// Estimate of confidence of the observation.
    confidence_t _confidence;

public:
    SimpleTruthValue(strength_t, confidence_t);
    SimpleTruthValue(const TruthValue&);
    SimpleTruthValue(SimpleTruthValue const&);

    virtual bool operator==(const ProtoAtom& rhs) const;

    std::string toString(const std::string&) const;

    strength_t getMean() const;
    count_t getCount() const;
    confidence_t getConfidence() const;

    /**
     * Truth value merge formula, as specified by PLN.
     *
     * Currently tv1.merge(tv2) works as follows:
     * the resulting TV is either tv1 or tv2, the result being the one
     * with the highest confidence.
     */
    TruthValuePtr merge(TruthValuePtr,
                        const MergeCtrl& mc=MergeCtrl());

    static SimpleTruthValuePtr createSTV(strength_t mean, confidence_t conf)
    {
        return std::make_shared<SimpleTruthValue>(mean, conf);
    }
    static TruthValuePtr createTV(strength_t mean, confidence_t conf)
    {
        return std::static_pointer_cast<TruthValue>(createSTV(mean, conf));
    }

    TruthValuePtr clone() const
    {
        return std::make_shared<SimpleTruthValue>(*this);
    }
    TruthValue* rawclone() const
    {
        return new SimpleTruthValue(*this);
    }
};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_SIMPLE_TRUTH_VALUE_H_
