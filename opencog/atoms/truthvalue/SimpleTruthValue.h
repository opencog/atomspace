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
};

VALUE_PTR_DECL(SimpleTruthValue);
CREATE_VALUE_DECL(SimpleTruthValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_SIMPLE_TRUTH_VALUE_H_
