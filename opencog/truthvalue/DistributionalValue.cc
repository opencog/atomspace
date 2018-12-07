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
#include <opencog/atoms/proto/FloatValue.h>
#include <opencog/atoms/proto/LinkValue.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

count_t DistributionalValue::DEFAULT_K = 800.0;

DistributionalValue::DistributionalValue()
    : ProtoAtom(DISTRIBUTIONAL_VALUE)
{}

DistributionalValue::DistributionalValue(ValueCounter hctr)
    : ProtoAtom(DISTRIBUTIONAL_VALUE)
{
    value = hctr;
}

DistributionalValue::DistributionalValue(double mode,double conf)
    : ProtoAtom(DISTRIBUTIONAL_VALUE)
{
    confidence_t cf = std::min(conf, 0.9999998);
    double count = (DEFAULT_K * cf / (1.0 - cf));
    ProtoAtomPtr h = createFloatValue(mode);
    value[h] = count;
}

DistributionalValuePtr DistributionalValue::createDV(ValueCounter hctr)
{
    return std::make_shared<const DistributionalValue>(hctr);
}

DistributionalValuePtr DistributionalValue::createDV(double mode
                                                    ,double conf)
{
    return std::make_shared<const DistributionalValue>(mode,conf);
}

DistributionalValuePtr DistributionalValue::UniformDistributionalValue(ProtoAtomPtr h,int c)
{
    ValueCounter hctr;
    hctr[h] = c;
    return std::make_shared<const DistributionalValue>(hctr);
}


DistributionalValuePtr DistributionalValue::UniformDistributionalValue(ProtomSeq hs,int c)
{
    ValueCounter hctr;
    for (ProtoAtomPtr h : hs)
    {
        hctr[h] = c;
    }
    return std::make_shared<const DistributionalValue>(hctr);
}

DistributionalValuePtr DistributionalValue::TRUE_TV()
{
    ProtoAtomPtr h1 = createFloatValue(1.0);
    ProtoAtomPtr h2 = createFloatValue(0.0);
    ValueCounter hc;
    hc[h1] = 1;
    hc[h2] = 0;
    return std::make_shared<const DistributionalValue>(hc);
}
DistributionalValuePtr DistributionalValue::FALSE_TV()
{
    ProtoAtomPtr h1 = createFloatValue(1.0);
    ProtoAtomPtr h2 = createFloatValue(0.0);
    ValueCounter hc;
    hc[h1] = 0;
    hc[h2] = 1;
    return std::make_shared<const DistributionalValue>(hc);
}
DistributionalValuePtr DistributionalValue::DEFAULT_TV()
{
    ProtoAtomPtr h1 = createFloatValue(1.0);
    ProtoAtomPtr h2 = createFloatValue(0.0);
    ValueCounter hc;
    hc[h1] = 0;
    hc[h2] = 0;
    return std::make_shared<const DistributionalValue>(hc);
}

double DistributionalValue::intervalDist(ProtoAtomPtr h1,ProtoAtomPtr h2)
{
	auto f = [](ProtomSeq::iterator it) {
		return FloatValueCast(*it)->value()[0];
	};

    if (h1->is_type(LINK_VALUE) && h2->is_type(LINK_VALUE))
	{
		ProtomSeq hs1 = LinkValueCast(h1)->value();
		ProtomSeq hs2 = LinkValueCast(h2)->value();
		if (hs1.size() != hs2.size())
			throw RuntimeException(TRACE_INFO,"Sizes don't match.");
		auto it1 = hs1.begin();
        auto it2 = hs2.begin();
        double max = 0;
        for (int i = 0; i < hs1.size();i++)
        {
            double n = abs(f(it1) - f(it2));
            if (n > max)
	            max = n;
            it1 = std::next(it1);
            it2 = std::next(it2);
        }
        return max;
	}
    else if (h1->is_type(FLOAT_VALUE) && h2->is_type(FLOAT_VALUE))
	{
		double v1 = FloatValueCast(h1)->value()[0];
		double v2 = FloatValueCast(h2)->value()[0];
		return abs(v1 - v2);
	}
    throw RuntimeException(TRACE_INFO,"Can't handle that case.");
}

ConditionalDVPtr DistributionalValue::divide(DistributionalValuePtr dv2,int i) const
{
	CDVrep res;
    for (auto elem : value)
    {
        ProtomSeq hs = LinkValueCast(elem.first)->value();
        if (hs.empty())
            throw RuntimeException(TRACE_INFO,"Can't divide non Joint DV.");
        ProtoAtomPtr h = hs[i];
        hs.erase(hs.begin() + i);
        ProtoAtomPtr interval;
        if (hs.size() != 1)
			interval = createLinkValue(hs);
        else
	        interval = hs[0];
        res[h][interval] = getMean(elem.first) / dv2->getMeanNoMatch(h)
                                               * dv2->total_count();
    }
    return ConditionalDV::createCDV(res);
}

DistributionalValuePtr DistributionalValue::SumJoint(DistributionalValuePtr dv,int pos) const
{
    ValueCounter res;
    for (auto elem : dv->value)
    {
        res += PartJoint(elem.first,pos);
    }
    return createDV(res);
}

ValueCounter DistributionalValue::PartJoint(ProtoAtomPtr idx,int pos) const
{
    ValueCounter res;
    for (auto elem : value)
    {
        ProtomSeq hs = LinkValueCast(elem.first)->value();
        if (hs[pos] == idx)
        {
            hs.erase(hs.begin() + pos);
            ProtoAtomPtr link = createLinkValue(hs);
            res[link] = elem.second;
        }
    }
    return res;
}

//Create a Conjuction from 2 DVs
DistributionalValuePtr DistributionalValue::Conjuction(DistributionalValuePtr dv) const
{
	typedef std::pair<ProtoAtomPtr,double> Elem;
	typedef std::function<bool(Elem,Elem)> Comparator;

	Comparator compF =
		[](Elem elem1,Elem elem2)
		{
			double v1 = getKeyMin(elem1.first);
			double v2 = getKeyMin(elem2.first);
			return v1 < v2;
		};

	std::set<Elem,Comparator> set1(value.begin(),value.end(),compF);
	std::set<Elem,Comparator> set2(dv->value.begin(),dv->value.end(),compF);

	ValueCounter res;
	double count = std::min(total_count(),dv->total_count());

	auto it1 = set1.begin();
	auto it2 = set2.begin();

	double m1 = 1;
	double m2 = 1;

	while (m1 != 0 && m2 != 0)
	{
		double v1 = getKeyMin(it1->first);
		double v2 = getKeyMin(it2->first);
		if (v1 < v2)
		{
			double mean = get_mean_for(it1->second);
			res[it1->first] += count * mean * m2;
			m1 -= mean;
			it1++;
		}
		else
		{
			double mean = get_mean_for(it2->second);
			res[it2->first] += count * mean * m1;
			m2 -= mean;
			it2++;
		}
	}

	return createDV(res);
}

//Create a disjuction from 2 DVs
DistributionalValuePtr DistributionalValue::Disjuction(DistributionalValuePtr dv) const
{
	typedef std::pair<ProtoAtomPtr,double> Elem;
	typedef std::function<bool(Elem,Elem)> Comparator;

	Comparator compF =
		[](Elem elem1,Elem elem2)
		{
			double v1 = getKeyMax(elem1.first);
			double v2 = getKeyMax(elem2.first);
			return v1 > v2;
		};

	std::set<Elem,Comparator> set1(value.begin(),value.end(),compF);
	std::set<Elem,Comparator> set2(dv->value.begin(),dv->value.end(),compF);

	ValueCounter res;
	double count = std::min(total_count(),dv->total_count());

	auto it1 = set1.begin();
	auto it2 = set2.begin();

	double m1 = 1;
	double m2 = 1;

	while (m1 != 0 && m2 != 0)
	{
		double v1 = getKeyMax(it1->first);
		double v2 = getKeyMax(it2->first);
		if (v1 > v2)
		{
			double mean = get_mean_for(it1->second);
			res[it1->first] += count * mean * m2;
			m1 -= mean;
			it1++;
		}
		else
		{
			double mean = get_mean_for(it2->second);
			res[it2->first] += count * mean * m1;
			m2 -= mean;
			it2++;
		}
	}

	return createDV(res);
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

DistributionalValuePtr DistributionalValue::AddEvidence(ProtoAtomPtr h) const
{
    ValueCounter newhc = value;
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
    ValueCounter newhc = value;
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
            it1 = std::next(it1);
            it2 = std::next(it2);
        }
        it1 = newhc.begin();
        it2 = newgdtv->value.begin();
        for (int i = 0; i < k;i++)
        {
            it1->second = it1->second + it2->second;
            it1 = std::next(it1);
            it2 = std::next(it2);
        }
    }
    return std::make_shared<const DistributionalValue>(newhc);
}

DistributionalValuePtr DistributionalValue::negate() const
{
    double total = maxCount() + minCount();
    ValueCounter res;
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
	double count = total_count();
	if (count == 0)
		return 0;
	else
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

double DistributionalValue::middleOfInterval(ProtoAtomPtr h) const
{
    if (h->get_type() == FLOAT_VALUE)
    {
        FloatValuePtr n = std::static_pointer_cast<FloatValue>(h);
        return n->value()[0];
    }
    LinkValuePtr l = std::static_pointer_cast<LinkValue>(h);
    ProtomSeq hs = l->value();
    FloatValuePtr n1 = std::static_pointer_cast<FloatValue>(hs[0]);
    FloatValuePtr n2 = std::static_pointer_cast<FloatValue>(hs[1]);
    return (n1->value()[0] + n2->value()[0]) / 2;
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

double DistributionalValue::get_swc() const
{
	double sum;
	for (auto elem : value)
	{
		sum += FloatValueCast(elem.first)->value()[0] * elem.second;
	}
    return sum;
}

double DistributionalValue::to_conf(int c)
{
    return c / (c + DEFAULT_K);
}

int DistributionalValue::to_count(double cf)
{
    return (cf * DEFAULT_K / (1 - cf));
}

ProtoAtomPtr DistributionalValue::getKey(ProtoAtomPtr h) const
{
    auto it = value.find(h);
    if (it != value.end())
    {
        return it->first;
    }
    throw RuntimeException(TRACE_INFO, "No Key for this value.");
}

double DistributionalValue::getKeyMin(ProtoAtomPtr p) const
{
	switch(p->get_type()) {
		case FLOAT_VALUE:
			return FloatValueCast(p)->value()[0];
		case LINK_VALUE:
			return FloatValueCast(LinkValueCast(p)->value()[0])->value()[0];
		default:
			throw RuntimeException(TRACE_INFO, "Can't handle key of this type.");
	}

}

double DistributionalValue::getKeyMax(ProtoAtomPtr p) const
{
	switch(p->get_type()) {
		case FLOAT_VALUE:
			return FloatValueCast(p)->value()[0];
		case LINK_VALUE:
			return FloatValueCast(LinkValueCast(p)->value()[1])->value()[0];
		default:
			throw RuntimeException(TRACE_INFO, "Can't handle key of this type.");
	}

}

ProtomSeq DistributionalValue::getKeys() const
{
    ProtomSeq res;
    for (auto h : value)
    {
        res.push_back(std::shared_ptr<ProtoAtom>(h.first));
    }
    return res;
}

double DistributionalValue::getCount(ProtoAtomPtr h) const
{
	for (auto& elem : value)
	{
		if (elem.first == h)
			return elem.second;
	}
    throw RuntimeException(TRACE_INFO, "No Key for this value.");
}

double DistributionalValue::getCountNoMatch(ProtoAtomPtr h) const
{
    double res;
    for (auto v : value)
    {
	    double dist = DistributionalValue::intervalDist(v.first,h);
        res += v.second * (1 - dist);
    }
    return res;
}

double DistributionalValue::getMode(ProtoAtomPtr val) const
{
    return get_mode_for(getCount(val));
}
double DistributionalValue::getMean(ProtoAtomPtr val) const
{
    return get_mean_for(getCount(val));
}
double DistributionalValue::getMeanNoMatch(ProtoAtomPtr val) const
{
    return get_mean_for(getCountNoMatch(val));
}
double DistributionalValue::getVar(ProtoAtomPtr val) const
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

bool DistributionalValue::operator<(const ProtoAtom& other) const
{
	if (DISTRIBUTIONAL_VALUE != other.get_type())
		return DISTRIBUTIONAL_VALUE < other.get_type();

	const DistributionalValue* dov = (const DistributionalValue*) &other;

	return value < dov->value;
}

std::string oc_to_string(const DistributionalValuePtr dvp)
{
    std::string part = dvp->to_string("");
    return part;
}
