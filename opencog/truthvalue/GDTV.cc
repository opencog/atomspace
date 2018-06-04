/*
 * opencog/truthvalue/GDTV.cc
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

#include <float.h>
#include <math.h>
#include <stdio.h>

#include <opencog/truthvalue/GDTV.h>

using namespace opencog;

GDTV::GDTV()
    : ProtoAtom(GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{}

GDTV::GDTV(HandleCounter hctr)
    : ProtoAtom(GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
    value = hctr;
}

GDTV::GDTV(SimpleTruthValue stv,AtomSpace as)
    : ProtoAtom(GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
    int count = std::round(stv.get_mean() * stv.get_count());
    Handle h = as.add_node(NUMBER_NODE,"1");
    value[h] = count;
}

GDTV::GDTV(FuzzyTruthValue ftv,AtomSpace as)
    : ProtoAtom(GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
    Handle h = as.add_node(NUMBER_NODE,std::to_string(ftv.get_mean()));
    value[h] = ftv.get_count();
}

GDTVPtr GDTV::UniformGDTV(std::vector<Handle> hs,int c)
{
    HandleCounter hctr;
    for (Handle h : hs)
    {
        hctr[h] = c;
    }
    return std::make_shared<const GDTV>(hctr);
}

void GDTV::AddEvidence(Handle h)
{
    auto it = value.find(h);
    if (it != value.end())
    {
        it->second = it->second + 1;
    }
    throw RuntimeException(TRACE_INFO, "No Interval for this value.");
}

void GDTV::AddEvidence(GDTVPtr newgdtv)
{
    if (k == newgdtv->k)
    {
        auto it1 = value.begin();
        auto it2 = newgdtv->value.begin();
        for (int i = 0; i < k;i++)
        {
            if (it1->first != it2->first)
            {
                throw RuntimeException(TRACE_INFO
                                      ,"Intervals of GDTVS don't match.");
            }
            std::next(it1);
            std::next(it2);
        }
        it1 = value.begin();
        it2 = newgdtv->value.begin();
        for (int i = 0; i < k;i++)
        {
            it1->second = it1->second + it2->second;
            std::next(it1);
            std::next(it2);
        }
    }
}

std::vector<double> GDTV::get_mode()
{
    std::vector<double> probs;
    for (auto elem : value)
    {
        probs.push_back(get_mode_for(elem.second));
    }
    return probs;
}

double GDTV::get_mode_for(double ai)
{
    return (ai - 1) / (total_count() - k);
}

std::vector<double> GDTV::get_mean()
{
    std::vector<double> probs;
    for (auto elem : value)
    {
        probs.push_back(get_mean_for(elem.second));
    }
    return probs;
}

double GDTV::get_mean_for(double ai)
{
        return ai / total_count();
}

std::vector<double> GDTV::get_var()
{
    std::vector<double> probs;
    for (auto elem : value)
    {
        probs.push_back(get_var_for(elem.second));
    }
    return probs;
}

double GDTV::get_var_for(double ai)
{
        double a0 = total_count();
        return ai*(a0-ai) / a0*a0*(a0+1);
}

double GDTV::total_count()
{
    return value.total_count();
}

double GDTV::get_confidence(int lookahead)
{
    int c = total_count();
    return c / (c + lookahead);
}

Handle GDTV::getKey(Handle h)
{
    auto it = value.find(h);
    if (it != value.end())
    {
        return it->first;
    }
    throw RuntimeException(TRACE_INFO, "No Key for this value.");
}

double GDTV::getCount(Handle h)
{
    auto it = value.find(h);
    if (it != value.end())
    {
        return it->second;
    }
    throw RuntimeException(TRACE_INFO, "No Key for this value.");
}

double GDTV::getMode(Handle val)
{
    return get_mode_for(getCount(val));
}
double GDTV::getMean(Handle val)
{
    return get_mean_for(getCount(val));
}
double GDTV::getVar(Handle val)
{
    return get_var_for(getCount(val));
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
