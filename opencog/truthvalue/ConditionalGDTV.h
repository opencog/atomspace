/*
 * opencog/truthvalue/ConditionalGDTV.h
 *
 * Copyright (C) 2018 SingularityNet
 * All Rights Reserved
 *
 * Written by Roman Treutlein
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

#ifndef _OPENCOG_CONDITIONAL_GDTV_H
#define _OPENCOG_CONDITIONAL_GDTV_H

#include <memory>
#include <string>
#include <vector>
#include <limits>

#include <opencog/util/exceptions.h>
#include <opencog/atoms/base/ProtoAtom.h>

#include <opencog/truthvalue/FuzzyTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/truthvalue/GDTV.h>
/** \addtogroup grp_atomspace
 *  @{
 */

namespace opencog
{

class ConditionalGDTV;
typedef std::shared_ptr<const ConditionalGDTV> ConditionalGDTVPtr;

class ConditionalGDTV
    : public ProtoAtom
{

    GDTVrep gdtv;

    // Disallow assignment -- truth values are immutable!
    ConditionalGDTV& operator=(const ConditionalGDTV& rhs) {
        throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
    }

public:
    GDTV(GDTVrep);

    GDTV getUnconditonal(double);
    //GDTV getUnconditonal(Interval);
    //GDTV getUnconditonal(GDTV);

};

} // namespace opencog

/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
