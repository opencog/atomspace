/*
 * opencog/truthvalue/TruthValue.h
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Guilherme Lamacie
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

#ifndef _OPENCOG_TRUTH_VALUE_H
#define _OPENCOG_TRUTH_VALUE_H

#include <memory>
#include <string>
#include <vector>

#include <opencog/util/exceptions.h>
#include <opencog/atoms/base/ProtoAtom.h>

/** \addtogroup grp_atomspace
 *  @{
 */

class TruthValueUTest;

namespace opencog
{

// Truth-value components
// For essentially all truth-value calculations, float is enough, so
// we save space here, and use float. For counting, a float is not
// enough -- it gets up to 16 million (24 bits) and then clamps. So
// we use a double for counting, which should provide 48 bits. Since
// SimpleTruthValue does not store count anyway, there is no storage
// penalty associated with this.
typedef float strength_t;
typedef float confidence_t;
typedef double count_t;

/// Class to control the TV merging strategy
struct MergeCtrl
{
    /// Styles for controlling merging two different truth value types
    /// as described in https://github.com/opencog/opencog/issues/1295
    ///
    /// Stronger means TV type with higher resolution. The weakest TV type
    /// would be simple TV, and the strongest would be distributional TV
    /// (to be (re)implemented).
    enum class TVType
    {
        OLDER,
        NEWER,
        STRONGER,
        WEAKER
    };

    /// Styles for controlling the formula while merging two different
    /// truth values.
    enum class TVFormula
    {
        HIGHER_CONFIDENCE,  // TV with higher confidence overwrite the other
        PLN_BOOK_REVISION   // PLN book Section 5.10.2 revision rule
    };

    TVFormula tv_formula;
    TVType tv_type;

    MergeCtrl(TVFormula tvf=TVFormula::PLN_BOOK_REVISION,
              TVType tvt=TVType::OLDER)
        : tv_formula(tvf), tv_type(tvt) {}
};

class TruthValue;
typedef std::shared_ptr<const TruthValue> TruthValuePtr;

class TruthValue
    : public ProtoAtom
{
    friend class Atom;

    // the TruthValueUTest class needs to access private members from the
    // TruthValue class, so we declare it as a friend class.
    friend class ::TruthValueUTest;

    // Disallow assignment -- truth values are immutable!
    TruthValue& operator=(const TruthValue& rhs) {
        throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
    }
public:
    // default lookahead
    static count_t DEFAULT_K;
    static void setDefaultK(count_t k) {
        DEFAULT_K = k;
    }

    TruthValue(Type t) : ProtoAtom(t) {}
    virtual ~TruthValue() {}

    std::string toShortString(const std::string&) const;

    // Special TVs

    /**
     * The shared reference to a special TRUE (Simple) TruthValue
     * object with MAX_TRUTH mean and MAX_TV_CONFIDENCE count. That is,
     * its true with absolute confidence.
     */
    static TruthValuePtr TRUE_TV();
    /**
     * The shared reference to a special default (Simple) TruthValue
     * object with MAX_TRUTH mean and 0 count.  That is, its true,
     * but with no confidence.
     */
    static TruthValuePtr DEFAULT_TV();
    /**
     * The shared reference to a special FALSE (Simple) TruthValue
     * object with 0 mean and MAX_TV_CONFIDENCE count. That is, its
     * false with absolute confidence.
     */
    static TruthValuePtr FALSE_TV();
    /**
     * The shared reference to a special TRIVIAL (Simple) TruthValue
     * object with 0 mean and 0 count. That is, its false, but with
     * no confidence.
     */
    static TruthValuePtr TRIVIAL_TV();

    virtual strength_t getMean()  const = 0;
    virtual confidence_t getConfidence()  const = 0;
    virtual count_t getCount()  const = 0;

    virtual TruthValuePtr clone() const  = 0;
    virtual TruthValue* rawclone() const  = 0;

    /**
     * Merge this TV object with the given TV object argument.
     * It always returns a new TV object with the result of the merge,
     * even if it is equal to one of the merged TV objects.
     * @param ms the merge style as described in
     *        https://github.com/opencog/opencog/issues/1295
     */
    virtual TruthValuePtr merge(TruthValuePtr,
                                const MergeCtrl& = MergeCtrl()) const = 0;

    /**
     * Check if this TV is equal to the default TV.
     * operator!= only compares pointers.
     */
    virtual bool isDefaultTV() const;
    virtual bool isDefinedTV() const;

protected:
    // Helper merging methods
    TruthValuePtr higher_confidence_merge(TruthValuePtr) const;
};

} // namespace opencog

// overload of operator<< to print TruthValue
namespace std
{
    template<typename Out>
    Out& operator<<(Out& out, const opencog::TruthValue& tv)
    {
        out << tv.toString("");
        return out;
    }
} // ~namespace std


/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
