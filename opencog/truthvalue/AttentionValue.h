/*
 * opencog/truthvalue/AttentionValue.h
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Tony Lofthouse <tony_lofthouse@btinternet.com>
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

#ifndef _OPENCOG_ATTENTION_VALUE_H
#define _OPENCOG_ATTENTION_VALUE_H

#include <limits.h>
#include <memory>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

//! The AttentionValue class stores attention in three components:
//! short-term, long-term and very long-term. It is immutable, in order
//! to provide thread-safety and atomic update.  Basically, if you have
//! a pointer to an AttentionValue, you are guaranteed that no one will
//! change it for as long as you are holding it. The only way to change
//! the AV on an atom is to replace it in it's entirety.

class AttentionValue;
typedef std::shared_ptr<const AttentionValue> AttentionValuePtr;
#define createAV std::make_shared<AttentionValue>

class AttentionValue
    : public std::enable_shared_from_this<AttentionValue>
{
public:
    typedef double sti_t;   //!< short-term importance type
    typedef double lti_t;   //!< long-term importance type
    typedef double vlti_t;  //!< very long-term importance type

    static const int DISPOSABLE = 0; //!< Status flag for vlti

    // CLASS CONSTANTS
    static const sti_t DEFAULTATOMSTI;   //!< short-term importance default
    static const lti_t DEFAULTATOMLTI;   //!< long-term importance default
    static const vlti_t DEFAULTATOMVLTI; //!< very long-term default

    static constexpr sti_t MAXSTI = SHRT_MAX;
    static constexpr lti_t MAXLTI = SHRT_MAX;
    static constexpr sti_t MINSTI = SHRT_MIN;
    static constexpr lti_t MINLTI = SHRT_MIN;

    //! to be used as default attention value
    static AttentionValuePtr DEFAULT_AV() {
        static AttentionValuePtr instance = createAV();
        return instance;
    }

private:
    //CLASS FIELDS
    sti_t m_STI;   //!< short-term importance
    lti_t m_LTI;   //!< long-term importance
    vlti_t m_VLTI; //!< represents the number of subsystems that need
                   //!< this atom to persist across system reboots.
                   //!< atoms with VLTI get saved to permanent storage.
public:
   /**
     * @param STI (int): The STI value to set for the atom
     * @param LTI (int): The LTI value to set for the atom
     * @param VLTI (unsigned short): The VLTI value to set for this atom
     */
    AttentionValue(sti_t STI = DEFAULTATOMSTI,
                   lti_t LTI = DEFAULTATOMLTI,
                   vlti_t VLTI = DEFAULTATOMVLTI)
        : m_STI(STI), m_LTI(LTI), m_VLTI(VLTI<0 ? 0 : VLTI) {}

    ~AttentionValue() {}

    //! return STI property value
    sti_t getSTI() const { return m_STI; }
    float getScaledSTI() const { return (((float) m_STI) + 32768) / 65534; }

    //! return LTI property value
    lti_t getLTI() const { return m_LTI; }

    //! return VLTI property value
    vlti_t getVLTI() const { return m_VLTI; }

    //! Returns const string "[sti_val, lti_val, vlti_val]"
    // @param none
    std::string toString() const;

    //! Returns An AttentionValue* cloned from this AttentionValue
    // @param none
    AttentionValuePtr clone() const { return createAV(m_STI, m_LTI, m_VLTI); }
    AttentionValue* rawclone() const { return new AttentionValue(m_STI, m_LTI, m_VLTI); }

    //! Compares two AttentionValues and returns true if the
    //! elements are equal false otherwise
    // @param none
    bool operator==(const AttentionValue& av) const {
        return (m_STI == av.getSTI() && m_LTI == av.getLTI() && m_VLTI == av.getVLTI());
    }
    inline bool operator!=(const AttentionValue& rhs) const
         { return !(*this == rhs); }

    bool isDefaultAV() const {
        if (this == DEFAULT_AV().get()) return true;
        if (*this == *DEFAULT_AV()) return true;
        return false;
    }
};


/** @}*/
} // namespace opencog

#endif // _OPENCOG_ATTENTION_VALUE_H
