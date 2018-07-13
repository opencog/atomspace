/*
 * opencog/truthvalue/ConditionalDV.cc
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

#include <opencog/truthvalue/ConditionalDV.h>
#include <opencog/truthvalue/DistributionalValue.h>
#include <opencog/atoms/base/Link.h>

using namespace opencog;

ConditionalDV::ConditionalDV()
: ProtoAtom(CONDITIONAL_DISTRIBUTIONAL_VALUE)
{
}

ConditionalDV::ConditionalDV(CDVrep rep)
: ProtoAtom(CONDITIONAL_DISTRIBUTIONAL_VALUE)
{
    value = rep;
}

ConditionalDV::ConditionalDV(HandleSeq conds,std::vector<DistributionalValuePtr> dvs)
: ProtoAtom(CONDITIONAL_DISTRIBUTIONAL_VALUE)
{
    auto it1 = conds.begin();
    auto it2 = dvs.begin();
    auto end1   = conds.end();
    auto end2   = dvs.end();
    for (;(it1 != end1) && (it2 != end2); ++it1, ++it2)
    {
        value[*it1] = (*it2)->value;
    }
}

ConditionalDVPtr ConditionalDV::createCDV()
{
    return std::make_shared<const ConditionalDV>();
}

ConditionalDVPtr ConditionalDV::createCDV(CDVrep rep)
{
    return std::make_shared<const ConditionalDV>(rep);
}

ConditionalDVPtr ConditionalDV::createCDV(HandleSeq conds
                                         ,std::vector<DistributionalValuePtr> dvs)
{
    return std::make_shared<const ConditionalDV>(conds,dvs);
}

HandleSeq ConditionalDV::getConditions() const
{
    HandleSeq res;
    for (auto gdtvpart : value)
    {
        res.push_back(gdtvpart.first);
    }
    return res;
}

std::vector<DistributionalValuePtr> ConditionalDV::getUnconditionals() const
{
    std::vector<DistributionalValuePtr> res;
    for (auto gdtvpart : value)
    {
        res.push_back(std::make_shared<const DistributionalValue>(gdtvpart.second));
    }
    return res;
}

DistributionalValuePtr ConditionalDV::getUnconditional(Handle h) const
{
    return std::make_shared<const DistributionalValue>(value.find(h)->second);
}

DistributionalValuePtr ConditionalDV::getUnconditional(DistributionalValuePtr condDist) const
{
    HandleCounter res;
    for (auto gdtvpart : value)
    {
        double val = condDist->getMean(gdtvpart.first);
        res += gdtvpart.second / gdtvpart.second.total_count() * val;
    }
    res = res * condDist->total_count();
    return std::make_shared<const DistributionalValue>(res);
}

DistributionalValuePtr ConditionalDV::getJointProbability(DistributionalValuePtr base) const
{
    HandleCounter res;
    HandleSeq ivsBASE = base->getKeys();
    HandleSeq ivsTHIS = DistributionalValue::createDV(value.begin()->second)->getKeys();
    for (auto h1 : ivsBASE) {
        for (auto h2 : ivsTHIS) {
            HandleSeq linkelems;
            if (h1->is_type(LIST_LINK))
            {
                HandleSeq hs = h1->getOutgoingSet();
                linkelems.insert(linkelems.end(),hs.begin(),hs.end());
            } else {
                linkelems.push_back(h1);
            }
            if (h2->is_type(LIST_LINK))
            {
                HandleSeq hs = h2->getOutgoingSet();
                linkelems.insert(linkelems.end(),hs.begin(),hs.end());
            } else {
                linkelems.push_back(h2);
            }

            Handle link = createLink(linkelems,LIST_LINK);
            //Res count based on base count
            //maybe should be based on base + this /2 or min of the two.
            res[link] = base->value.at(h1)
                      * (value.at(h1).at(h2)/value.at(h1).total_count());
        }
    }
    return std::make_shared<const DistributionalValue>(res);
}

std::string ConditionalDV::to_string(const std::string& indent) const
{
    std::stringstream ss;
    for (auto elem : value)
    {
        ss << indent
           << elem.first->to_string()
           << " DV: "
           << std::endl
           << DistributionalValue(elem.second).to_string(indent + "    ")
           << std::endl;
    }
    return ss.str();
}

bool ConditionalDV::operator==(const ProtoAtom& rhs) const
{
    throw RuntimeException(TRACE_INFO, "Not Implemented");

    const DistributionalValue *gdtv2 = dynamic_cast<const DistributionalValue *>(&rhs);
    if (NULL == gdtv2) return false;
}
