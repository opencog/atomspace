/*
 * opencog/truthvalue/DistributionalValue.cc
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

#include <opencog/truthvalue/DistributionalValue.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

count_t DistributionalValue::DEFAULT_K = 800.0;

DistributionalValue::DistributionalValue()
    : ProtoAtom(DISTRIBUTIONAL_VALUE)
{}

DistributionalValue::DistributionalValue(HandleCounter hctr)
    : ProtoAtom(DISTRIBUTIONAL_VALUE)
{
    value = hctr;
}

DistributionalValue::DistributionalValue(double mode,double conf)
    : ProtoAtom(DISTRIBUTIONAL_VALUE)
{
    confidence_t cf = std::min(conf, 0.9999998);
    double count = (DEFAULT_K * cf / (1.0 - cf));
    Handle h = createNode(NUMBER_NODE,std::to_string(mode));
    value[h] = count;
}

DistributionalValuePtr DistributionalValue::createDV(HandleCounter hctr)
{
    return std::make_shared<const DistributionalValue>(hctr);
}

DistributionalValuePtr DistributionalValue::createDV(double mode
                                                    ,double conf)
{
    return std::make_shared<const DistributionalValue>(mode,conf);
}

DistributionalValuePtr DistributionalValue::UniformDistributionalValue(Handle h,int c)
{
    HandleCounter hctr;
    hctr[h] = c;
    return std::make_shared<const DistributionalValue>(hctr);
}


DistributionalValuePtr DistributionalValue::UniformDistributionalValue(HandleSeq hs,int c)
{
    HandleCounter hctr;
    for (Handle h : hs)
    {
        hctr[h] = c;
    }
    return std::make_shared<const DistributionalValue>(hctr);
}

DistributionalValuePtr DistributionalValue::TRUE_TV()
{
    Handle h1 = createNode(NUMBER_NODE,"1");
    Handle h2 = createNode(NUMBER_NODE,"0");
    HandleCounter hc;
    hc[h1] = 1;
    hc[h2] = 0;
    return std::make_shared<const DistributionalValue>(hc);
}
DistributionalValuePtr DistributionalValue::FALSE_TV()
{
    Handle h1 = createNode(NUMBER_NODE,"1");
    Handle h2 = createNode(NUMBER_NODE,"0");
    HandleCounter hc;
    hc[h1] = 0;
    hc[h2] = 1;
    return std::make_shared<const DistributionalValue>(hc);
}
DistributionalValuePtr DistributionalValue::DEFAULT_TV()
{
    Handle h1 = createNode(NUMBER_NODE,"1");
    Handle h2 = createNode(NUMBER_NODE,"0");
    HandleCounter hc;
    hc[h1] = 0;
    hc[h2] = 0;
    return std::make_shared<const DistributionalValue>(hc);
}

ConditionalDVPtr DistributionalValue::divide(DistributionalValuePtr dv2,int i) const
{
    CDVrep res;
    for (auto elem : value)
    {
        HandleSeq hs = elem.first->getOutgoingSet();
        if (hs.empty())
            throw RuntimeException(TRACE_INFO,"Can't divide non Joint DV.");
        Handle h = hs[i];
        hs.erase(hs.begin() + i);
        Handle link = createLink(hs,LIST_LINK);
        res[h][link] = getMean(elem.first) / dv2->getMean(h) * dv2->total_count();
    }
    return ConditionalDV::createCDV(res);
}

DistributionalValuePtr DistributionalValue::SumJoint(DistributionalValuePtr dv,int pos) const
{
    HandleCounter res;
    for (auto elem : dv->value)
    {
        res += PartJoint(elem.first,pos);
    }
    return createDV(res);
}

HandleCounter DistributionalValue::PartJoint(Handle idx,int pos) const
{
    HandleCounter res;
    for (auto elem : value)
    {
        HandleSeq hs = elem.first->getOutgoingSet();
        if (hs[pos] == idx)
        {
            hs.erase(hs.begin() + pos);
            Handle link = createLink(hs,LIST_LINK);
            res[link] = elem.second;
        }
    }
    return res;
}

bool DistributionalValue::IsUniform() const
{
    double val = value.begin()->second;
    for (auto p : value)
    {
        if (val != p.second)
        {
            return false;
        }
    }
    return true;
}

DistributionalValuePtr DistributionalValue::AddEvidence(Handle h) const
{
    HandleCounter newhc = value;
    auto it = newhc.find(h);
    if (it != newhc.end())
    {
        it->second = it->second + 1;
        return std::make_shared<const DistributionalValue>(newhc);
    }
    throw RuntimeException(TRACE_INFO, "No Interval for this value.");
}

DistributionalValuePtr DistributionalValue::merge(DistributionalValuePtr newgdtv) const
{
    HandleCounter newhc = value;
    if (k == newgdtv->k)
    {
        auto it1 = newhc.begin();
        auto it2 = newgdtv->value.begin();
        for (int i = 0; i < k;i++)
        {
            if (it1->first != it2->first)
            {
                throw RuntimeException(TRACE_INFO
                                      ,"Intervals of DistributionalValueS don't match.");
            }
            std::next(it1);
            std::next(it2);
        }
        it1 = newhc.begin();
        it2 = newgdtv->value.begin();
        for (int i = 0; i < k;i++)
        {
            it1->second = it1->second + it2->second;
            std::next(it1);
            std::next(it2);
        }
    }
    return std::make_shared<const DistributionalValue>(newhc);
}

DistributionalValuePtr DistributionalValue::negate() const
{
    double total = maxCount() + minCount();
    HandleCounter res;
    for (auto elem : value)
    {
        res[elem.first] = total - elem.second;
    }
    return std::make_shared<const DistributionalValue>(res);
}

double DistributionalValue::minCount() const
{
    double min = std::numeric_limits<double>::max();
    for (auto elem : value)
    {
        if (min >= elem.second)
            min = elem.second;
    }
    return min;
}
double DistributionalValue::maxCount() const
{
    double max = 0;
    for (auto elem : value)
    {
        if (max <= elem.second)
            max = elem.second;
    }
    return max;
}

std::vector<double> DistributionalValue::get_mode() const
{
    std::vector<double> probs;
    for (auto elem : value)
    {
        probs.push_back(get_mode_for(elem.second));
    }
    return probs;
}

double DistributionalValue::get_mode_for(double ai) const
{
    return (ai - 1) / (total_count() - k);
}

std::vector<double> DistributionalValue::get_mean() const
{
    std::vector<double> probs;
    for (auto elem : value)
    {
        probs.push_back(get_mean_for(elem.second));
    }
    return probs;
}

double DistributionalValue::get_mean_for(double ai) const
{
        return ai / total_count();
}

double DistributionalValue::get_fstord_mean() const
{
    double res = 0;
    for (auto elem : value)
    {
        //Is mode correct or should i use mean?
        res = res + middleOfInterval(elem.first) * get_mode_for(elem.second);
    }
    return res;
}

double DistributionalValue::middleOfInterval(Handle h) const
{
    if (h->get_type() == NUMBER_NODE)
    {
        NumberNodePtr n = std::static_pointer_cast<NumberNode>(h);
        return n->get_value();
    }
    LinkPtr l = std::static_pointer_cast<Link>(h);
    HandleSeq hs = l->getOutgoingSet();
    NumberNodePtr n1 = std::static_pointer_cast<NumberNode>(hs[0]);
    NumberNodePtr n2 = std::static_pointer_cast<NumberNode>(hs[1]);
    return (n1->get_value() + n2->get_value()) / 2;
}

std::vector<double> DistributionalValue::get_var() const
{
    std::vector<double> probs;
    for (auto elem : value)
    {
        probs.push_back(get_var_for(elem.second));
    }
    return probs;
}

double DistributionalValue::get_var_for(double ai) const
{
        double a0 = total_count();
        return ai*(a0-ai) / a0*a0*(a0+1);
}

double DistributionalValue::total_count() const
{
    return value.total_count();
}

double DistributionalValue::get_confidence() const
{
    int c = total_count();
    return c / (c + DEFAULT_K);
}

double DistributionalValue::to_conf(int c)
{
    return c / (c + DEFAULT_K);
}

int DistributionalValue::to_count(double cf)
{
    return (cf * DEFAULT_K / (1 - cf));
}

Handle DistributionalValue::getKey(Handle h) const
{
    auto it = value.find(h);
    if (it != value.end())
    {
        return it->first;
    }
    throw RuntimeException(TRACE_INFO, "No Key for this value.");
}

HandleSeq DistributionalValue::getKeys() const
{
    HandleSeq res;
    for (auto h : value)
    {
        res.push_back(h.first);
    }
    return res;
}

double DistributionalValue::getCount(Handle h) const
{
    auto it = value.find(h);
    if (it != value.end())
    {
        return it->second;
    }
    throw RuntimeException(TRACE_INFO, "No Key for this value.");
}

double DistributionalValue::getMode(Handle val) const
{
    return get_mode_for(getCount(val));
}
double DistributionalValue::getMean(Handle val) const
{
    return get_mean_for(getCount(val));
}
double DistributionalValue::getVar(Handle val) const
{
    return get_var_for(getCount(val));
}

std::string DistributionalValue::to_string(const std::string& indent) const
{
    std::stringstream ss;
    for (auto elem : value)
    {
        ss << elem.first->to_string(indent)
           << indent
           << " Count: "
           << elem.second
           << std::endl;
    }
    return ss.str();
}

bool DistributionalValue::operator==(const ProtoAtom& rhs) const
{
    throw RuntimeException(TRACE_INFO, "Not Implemented");

    const DistributionalValue *gdtv2 = dynamic_cast<const DistributionalValue *>(&rhs);
    if (NULL == gdtv2) return false;
}

std::string oc_to_string(const DistributionalValuePtr dvp)
{
    std::string part = dvp->to_string("");
    return part;
}
