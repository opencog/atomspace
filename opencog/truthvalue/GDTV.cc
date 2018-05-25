/*
 * opencog/truthvalue/GDTV.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
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

#include <float.h>
#include <math.h>
#include <stdio.h>

#include <opencog/truthvalue/GDTV.h>

using namespace opencog;

GDTV::GDTV()
    : ProtoAtom(GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{}

GDTV::GDTV(IntervalCounts ics)
    : ProtoAtom(GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
    gdtv = ics;
}

GDTV::GDTV(SimpleTruthValue stv)
    : ProtoAtom(GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
    int count = std::round(stv.get_mean() * stv.get_count());
    Interval interval = std::make_tuple(1.0,1.0);
    IntervalCount intervalWithCount = std::make_tuple(interval,count);
    gdtv.push_back(intervalWithCount);
}

GDTV::GDTV(FuzzyTruthValue ftv)
    : ProtoAtom(GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
    Interval interval = std::make_tuple(ftv.get_mean(),ftv.get_mean());
    IntervalCount intervalWithCount = std::make_tuple(interval,ftv.get_count());
    gdtv.push_back(intervalWithCount);
}

GDTVPtr UniformGDTV(int k,int c)
{
    IntervalCounts ics;
    for (int i = 0; i < k; i++)
    {
        Interval interval = std::make_tuple(i,i);
        IntervalCount intervalWithCount = std::make_tuple(interval,c);
        ics.push_back(intervalWithCount);
    }
    return std::make_shared<const GDTV>(ics);
}

GDTVPtr GDTV::UniformGDTV(std::vector<Interval> k,int c)
{
    IntervalCounts ics;
    for (size_t i = 0; i < k.size(); i++)
    {
        IntervalCount intervalWithCount = std::make_tuple(k[i],c);
        ics.push_back(intervalWithCount);
    }
    return std::make_shared<const GDTV>(ics);
}

GDTVPtr GDTV::UniformGDTV(IntervalCounts ics,int c)
{
    IntervalCounts res;
    for (auto intervalWithCount : ics)
    {
        Interval interval = std::get<0>(intervalWithCount);
        IntervalCount tuple = std::make_tuple(interval,c);
        res.push_back(tuple);
    }
    return std::make_shared<const GDTV>(res);
}

void GDTV::AddEvidence(double val)
{
    for (auto &intervalWithCount : gdtv)
    {
        Interval interval = std::get<0>(intervalWithCount);

        if (std::get<0>(interval) <= val &&  val <= std::get<1>(interval))
        {
            int count = std::get<1>(intervalWithCount);
            intervalWithCount = std::make_tuple(interval,count+1);
            break;
        }
    }
    throw RuntimeException(TRACE_INFO, "No Interval for this value.");
}

void GDTV::AddEvidence(GDTVPtr newgdtv)
{
    for (int i = 0; i < k;i++)
    {
        if (std::get<0>(gdtv[i]) != std::get<0>(newgdtv->gdtv[i]))
        {
            throw RuntimeException(TRACE_INFO,"Intervals of GDTVS don't match.");
        }
    }
    for (int i = 0; i < k;i++)
    {
        std::get<1>(gdtv[i]) = std::get<1>(gdtv[i]) + std::get<1>(newgdtv->gdtv[i]);
    }
}

std::vector<double> GDTV::get_mode()
{
    std::vector<double> probs;
    for (auto intervalWithCount : gdtv)
    {
        probs.push_back(get_mode_for(intervalWithCount));
    }
    return probs;
}

double GDTV::get_mode_for(IntervalCount ic)
{
    double ai = std::get<1>(ic);
    double a0 = get_count();
    return (ai - 1) / (a0 - k);
}

std::vector<double> GDTV::get_mean()
{
    std::vector<double> probs;
    for (auto intervalWithCount : gdtv)
    {
        probs.push_back(get_mean_for(intervalWithCount));
    }
    return probs;
}

double GDTV::get_mean_for(IntervalCount ic)
{
        double ai = std::get<1>(ic);
        double a0 = get_count();
        return ai / a0;
}

std::vector<double> GDTV::get_var()
{
    std::vector<double> probs;
    for (auto intervalWithCount : gdtv)
    {
        probs.push_back(get_var_for(intervalWithCount));
    }
    return probs;
}

double GDTV::get_var_for(IntervalCount ic)
{
        double ai = std::get<1>(ic);
        double a0 = get_count();
        return ai*(a0-ai) / a0*a0*(a0+1);
}

int GDTV::get_count()
{
    int sum = 0;
    for (auto intervalWithCount : gdtv)
    {
        sum = sum + std::get<1>(intervalWithCount);
    }
    return sum;
}

double GDTV::get_confidence(int lookahead)
{
    int c = get_count();
    return c / (c + lookahead);
}

IntervalCount GDTV::getIntervalCount(double val)
{
    for (auto intervalWithCount : gdtv)
    {
        Interval interval = std::get<0>(intervalWithCount);

        if (std::get<0>(interval)<= val &&  val <= std::get<1>(interval))
        {
            return intervalWithCount;
        }
    }
    throw RuntimeException(TRACE_INFO, "No Interval for this value.");
}

Interval GDTV::getInterval(double val)
{
    return std::get<0>(getIntervalCount(val));
}
int GDTV::getCount(double val)
{
    return std::get<1>(getIntervalCount(val));
}
double GDTV::getMode(double val)
{
    return get_mode_for(getIntervalCount(val));
}
double GDTV::getMean(double val)
{
    return get_mean_for(getIntervalCount(val));
}
double GDTV::getVar(double val)
{
    return get_var_for(getIntervalCount(val));
}

std::string GDTV::to_string(const std::string& indent) const
{
    throw RuntimeException(TRACE_INFO, "Not Implemented");
}

bool GDTV::operator==(const ProtoAtom& rhs) const
{
    throw RuntimeException(TRACE_INFO, "Not Implemented");

    const GDTV *gdtv2 = dynamic_cast<const GDTV *>(&rhs);
    if (NULL == gdtv2) return false;

}
