/*
 * opencog/atoms/truthvalue/IndefiniteTruthValue.h
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Welter Silva <welter@vettalabs.com>
 *            Fabricio Silva <fabricio@vettalabs.com>
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

#ifndef _OPENCOG_INDEFINITE_TRUTH_VALUE_H
#define _OPENCOG_INDEFINITE_TRUTH_VALUE_H

#include <vector>

#include <opencog/atoms/truthvalue/TruthValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

// XXX FIXME Is this actually used anywehere for anything?
// Can we get rid of it?

class IndefiniteTruthValue;
typedef std::shared_ptr<const IndefiniteTruthValue> IndefiniteTruthValuePtr;


static inline IndefiniteTruthValuePtr IndefiniteTVCast(TruthValuePtr tv)
    { return std::dynamic_pointer_cast<const IndefiniteTruthValue>(tv); }

/**
 * Indefinite probabilities are in the form ([L,U],b,N). In practical work,
 * N will be hold constant and thus we have only ([L,U],b).
 */
class IndefiniteTruthValue : public TruthValue
{
private:
    enum {
        MEAN,
        U,
        L,
        CONFIDENCE_LEVEL //!< referred as "b" in the paper
    };
    bool symmetric;

    //! used in inference rule procedure in order to compute L1 and U1
    //! when diff is negative it means that it is outdated and must be
    //! recalculated
    strength_t diff;

	/** @name mean, count, confidence
     * below, "mean", "count" and "confidence" are all three attributes
     * used to translate an indefinite TV into a simple TV.
     * If "mean" (as well as "count" or "confidence") is negative
     * it means that it outdated and therefore must be recalculted,
     * otherwise, i.e. positive of null, it means that the current value
     * is correct
	 */
	///@{
    count_t count;
    confidence_t confidence;
	///@}

    std::vector<strength_t*> firstOrderDistribution;

    void init(strength_t l = 0.0f, strength_t u = 0.0f,
              confidence_t c = DEFAULT_CONFIDENCE_LEVEL);
    void copy(const IndefiniteTruthValue&);

    //! find diff by dichotomy
    strength_t findDiff(strength_t idiff) const;

public:
    static count_t DEFAULT_K;
    static void setDefaultK(count_t k) {
        DEFAULT_K = k;
    }


    IndefiniteTruthValue(const std::vector<double>&);
    IndefiniteTruthValue();
    IndefiniteTruthValue(strength_t l, strength_t u,
                         confidence_t c = DEFAULT_CONFIDENCE_LEVEL);
    IndefiniteTruthValue(IndefiniteTruthValue const&);
    IndefiniteTruthValue(const ValuePtr&);

    //! it is a strict equality comparison, without error interval tolerance
    virtual bool operator==(const Value&) const;

    strength_t get_mean() const { return _value[MEAN]; }
    strength_t getU() const { return _value[U]; }
    strength_t getL() const { return _value[L]; }
    confidence_t getConfidenceLevel() const { return _value[CONFIDENCE_LEVEL]; }
    strength_t getDiff() const { return diff; }
    const std::vector<strength_t*>& getFirstOrderDistribution() const;

    count_t get_count() const { return count; }
    confidence_t get_confidence() const { return confidence; }
    strength_t getU_() const { return _value[U] + diff; }
    strength_t getL_() const { return _value[L] - diff; }
    bool isSymmetric() const { return symmetric; }

    TruthValuePtr merge(const TruthValuePtr&,
                        const MergeCtrl& mc=MergeCtrl()) const;

    std::string to_string(const std::string&) const;

    // clone method
    static IndefiniteTruthValuePtr createITV(const TruthValuePtr& tv)
    {
        if (tv->get_type() != INDEFINITE_TRUTH_VALUE)
            throw RuntimeException(TRACE_INFO, "Cannot clone non-indefinite TV");
        return std::make_shared<IndefiniteTruthValue>(
            static_cast<const IndefiniteTruthValue&>(*tv));
    }

    static TruthValuePtr createTV(const TruthValuePtr& tv)
    {
        return std::static_pointer_cast<const TruthValue>(createITV(tv));
    }

    static IndefiniteTruthValuePtr createITV(strength_t l, strength_t u,
                         confidence_t c = DEFAULT_CONFIDENCE_LEVEL)
    {
        return std::make_shared<const IndefiniteTruthValue>(l, u, c);
    }

    static TruthValuePtr createTV(strength_t l, strength_t u,
                         confidence_t c = DEFAULT_CONFIDENCE_LEVEL)
    {
        return std::static_pointer_cast<const TruthValue>(createITV(l, u, c));
    }

    static TruthValuePtr createTV(const ValuePtr& pap)
    {
        return std::static_pointer_cast<const TruthValue>(
            std::make_shared<const IndefiniteTruthValue>(pap));
    }

    static TruthValuePtr createTV(const std::vector<double>& v)
    {
        return std::static_pointer_cast<const TruthValue>(
            std::make_shared<const IndefiniteTruthValue>(v));
    }

    static confidence_t DEFAULT_CONFIDENCE_LEVEL;
    static strength_t diffError;
    static strength_t s; //Nil : not that sure s should be strength_t
    static void setDefaultConfidenceLevel(confidence_t c) {
        DEFAULT_CONFIDENCE_LEVEL = c;
    }
};

template<typename ... Type>
static inline TruthValuePtr createIndefiniteTruthValue(Type&&...  args) {
   return IndefiniteTruthValue::createTV(std::forward<Type>(args)...);
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_INDEFINITE_TRUTH_VALUE_H
