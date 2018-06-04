/*
 * opencog/truthvalue/GDTV.h
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

#ifndef _OPENCOG_GDTV_H
#define _OPENCOG_GDTV_H

#include <memory>
#include <string>
#include <vector>
#include <limits>

#include <opencog/util/exceptions.h>
#include <opencog/atoms/base/ProtoAtom.h>
#include <opencog/atomspace/AtomSpace.h>

#include <opencog/truthvalue/FuzzyTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/TruthValue.h>
/** \addtogroup grp_atomspace
 *  @{
 */

namespace opencog
{

class GDTV;
typedef std::shared_ptr<const GDTV> GDTVPtr;

typedef std::tuple<Handle,HandleCounter> GDTVpart;
typedef std::vector<GDTVpart> GDTVrep;

class GDTV
    : public ProtoAtom
{
    HandleCounter value;
    int k;

    // Disallow assignment -- truth values are immutable!
    GDTV& operator=(const GDTV& rhs) {
        throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
    }

public:
    GDTV();
    GDTV(HandleCounter);
    GDTV(SimpleTruthValue,AtomSpace);
    GDTV(FuzzyTruthValue,AtomSpace);

    static GDTVPtr UniformGDTV(std::vector<Handle>,int);

    std::vector<double> get_mode();
    std::vector<double> get_mean();
    std::vector<double> get_var();

    double get_mode_for(double);
    double get_mean_for(double);
    double get_var_for(double);

    void AddEvidence(Handle);
    void AddEvidence(GDTVPtr);

    double total_count();
    double get_confidence(int);

    Handle getKey(Handle);
    double getCount(Handle);
    double getMean(Handle);
    double getMode(Handle);
    double getVar(Handle);

    virtual bool operator==(const ProtoAtom& rhs) const;

    std::string to_string(const std::string&) const;
};

} // namespace opencog

/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
