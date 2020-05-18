/*
 * opencog/atoms/truthvalue/SimpleTruthValue.h
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

#include <opencog/atoms/truthvalue/TruthValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class SimpleTruthValue;
typedef std::shared_ptr<const SimpleTruthValue> SimpleTruthValuePtr;

//! a TruthValue that stores a strength and confidence.
class SimpleTruthValue : public TruthValue
{
protected:
    enum {
        MEAN, /// Mean of the strength of the TV over all observations.
        CONFIDENCE /// Estimate of confidence of the observation.
    };

public:
    static count_t DEFAULT_K;

    SimpleTruthValue(const std::vector<double>&);
    SimpleTruthValue(strength_t, confidence_t);
    SimpleTruthValue(const TruthValue&);
    SimpleTruthValue(const SimpleTruthValue&);
    SimpleTruthValue(const ValuePtr&);

    virtual bool operator==(const Value& rhs) const;

    std::string to_string(const std::string& = "") const;

    virtual strength_t get_mean() const;
    virtual count_t get_count() const;
    virtual confidence_t get_confidence() const;

    /**
     * Truth value merge formula, as specified by PLN.
     *
     * Currently tv1.merge(tv2) works as follows:
     * the resulting TV is either tv1 or tv2, the result being the one
     * with the highest confidence.
     */
    TruthValuePtr merge(const TruthValuePtr&,
                        const MergeCtrl& mc=MergeCtrl()) const;

    // XXX FIXME Are all of these really needed?
    // Can we get rid of some of them?
    static SimpleTruthValuePtr createSTV(strength_t mean, confidence_t conf)
    {
        return std::make_shared<const SimpleTruthValue>(mean, conf);
    }
    static TruthValuePtr createTV(strength_t mean, confidence_t conf)
    {
        return std::static_pointer_cast<const TruthValue>(createSTV(mean, conf));
    }
    static TruthValuePtr createTV(const std::vector<double>& v)
    {
        return std::static_pointer_cast<const TruthValue>(
            std::make_shared<const SimpleTruthValue>(v));
    }

    static TruthValuePtr createTV(const ValuePtr& pap)
    {
        return std::static_pointer_cast<const TruthValue>(
            std::make_shared<const SimpleTruthValue>(pap));
    }
};

static inline SimpleTruthValuePtr SimpleTruthValueCast(const ValuePtr& pa)
    { return std::dynamic_pointer_cast<const SimpleTruthValue>(pa); }

template<typename ... Type>
static inline TruthValuePtr createSimpleTruthValue(Type&&...  args) {
   return SimpleTruthValue::createTV(std::forward<Type>(args)...);
}


/** @}*/
} // namespace opencog

#endif // _OPENCOG_SIMPLE_TRUTH_VALUE_H_
