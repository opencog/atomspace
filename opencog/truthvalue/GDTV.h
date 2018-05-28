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

typedef std::tuple<double,double> Interval;
typedef std::tuple<Interval,int> IntervalCount;
typedef std::vector<IntervalCount> IntervalCounts;
typedef std::tuple<Interval,IntervalCounts> GDTVpart;
typedef std::vector<GDTVpart> GDTVrep;

class GDTV
    : public ProtoAtom
{

    IntervalCounts gdtv;
    int k;

    // Disallow assignment -- truth values are immutable!
    GDTV& operator=(const GDTV& rhs) {
        throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
    }

public:
    GDTV();
    GDTV(IntervalCounts);
    GDTV(SimpleTruthValue);
    GDTV(FuzzyTruthValue);

    static GDTVPtr UniformGDTV(int,int);
    static GDTVPtr UniformGDTV(std::vector<Interval>,int);
    static GDTVPtr UniformGDTV(IntervalCounts,int);

    Interval InfInterval() {
        double inf = std::numeric_limits<double>::infinity();
        return std::make_tuple(-inf,inf);
    }

    std::vector<double> get_mode();
    std::vector<double> get_mean();
    std::vector<double> get_var();

    double get_mode_for(IntervalCount);
    double get_mean_for(IntervalCount);
    double get_var_for(IntervalCount);

    void AddEvidence(double);
    void AddEvidence(GDTVPtr);

    int get_count();
    double get_confidence(int);

    IntervalCount getIntervalCount(double);
    Interval getInterval(double);
    int getCount(double);
    double getMean(double);
    double getMode(double);
    double getVar(double);

    virtual bool operator==(const ProtoAtom& rhs) const;

    std::string to_string(const std::string&) const;
};

} // namespace opencog

/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
