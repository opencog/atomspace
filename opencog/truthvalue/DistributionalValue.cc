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

#include <opencog/util/numeric.h>

#include <opencog/truthvalue/DistributionalValue.h>
#include <opencog/atoms/proto/FloatValue.h>
#include <opencog/atoms/proto/LinkValue.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomspace/AtomSpace.h>

#include <boost/range/combine.hpp>
#include <iomanip>

using namespace opencog;

count_t DistributionalValue::DEFAULT_K = 800.0;

DistributionalValue::DistributionalValue()
	: ProtoAtom(DISTRIBUTIONAL_VALUE)
{}

DistributionalValue::DistributionalValue(DVCounter dvctr)
	: ProtoAtom(DISTRIBUTIONAL_VALUE)
{
	for (auto elem : dvctr)
	{
		if (elem.second <= 0)
			throw std::invalid_argument("DV cant have an element with count 0.");
	}
	_value = dvctr;
}

DistributionalValue::DistributionalValue(double mode,double conf)
	: ProtoAtom(DISTRIBUTIONAL_VALUE)
{
	confidence_t cf = std::min(conf, 0.9999998);
	double count = (DEFAULT_K * cf / (1.0 - cf));
	//DV Interval with count 0 is undefined
	count = std::max(count, 0.0000002);
	DVKey k{Interval{mode}};
	_value[k] = count;
}

DistributionalValuePtr DistributionalValue::createDV(DVCounter dvctr)
{
	return std::make_shared<const DistributionalValue>(dvctr);
}

DistributionalValuePtr DistributionalValue::createDV(double mode
													,double conf)
{
	return std::make_shared<const DistributionalValue>(mode,conf);
}

DistributionalValuePtr DistributionalValue::UniformDistributionalValue(DVKey k,int c)
{
	DVCounter dvctr;
	dvctr[k] = c;
	return createDV(dvctr);
}


DistributionalValuePtr DistributionalValue::UniformDistributionalValue(DVKeySeq ks,int c)
{
	DVCounter dvctr;
	for (auto k : ks)
	{
		dvctr[k] = c;
	}
	return createDV(dvctr);
}

DistributionalValuePtr DistributionalValue::TRUE_TV()
{
	DVKey v1{Interval{1.0}};
	DVKey v2{Interval{0.0}};
	DVCounter dvc;
	dvc[v1] = 1;
	dvc[v2] = 0;
	return std::make_shared<const DistributionalValue>(dvc);
}
DistributionalValuePtr DistributionalValue::FALSE_TV()
{
	DVKey v1{Interval{1.0}};
	DVKey v2{Interval{0.0}};
	DVCounter dvc;
	dvc[v1] = 0;
	dvc[v2] = 1;
	return std::make_shared<const DistributionalValue>(dvc);
}
DistributionalValuePtr DistributionalValue::DEFAULT_TV()
{
	DVKey v1{Interval{1.0}};
	DVKey v2{Interval{0.0}};
	DVCounter dvc;
	dvc[v1] = 0;
	dvc[v2] = 0;
	return std::make_shared<const DistributionalValue>(dvc);
}

bool DistributionalValue::is_uniform() const
{
	double val = _value.begin()->second;
	for (auto p : _value)
	{
		if (val != p.second)
		{
			return false;
		}
	}
	return true;
}

DistributionalValuePtr DistributionalValue::add_evidence(DVKey h) const
{
	//TODO: should this really be const??? also:
	//There was a check for the existen of the Key
	//But i think we should be able to add new keys using this
	DVCounter newdvc = _value;
	newdvc[h] += 1;
	return createDV(newdvc);
}

DistributionalValuePtr DistributionalValue::merge(DistributionalValuePtr other) const
{
	DVCounter newdvc = _value;
	newdvc += other->_value;
	return createDV(newdvc);
}

DistributionalValuePtr DistributionalValue::negate() const
{
	double total = max_count() + min_count();
	DVCounter res;
	for (auto elem : _value)
	{
		res[elem.first] = total - elem.second;
	}
	return createDV(res);
}

double DistributionalValue::min_count() const
{
	double min = std::numeric_limits<double>::max();
	for (auto elem : _value)
	{
		if (min >= elem.second)
			min = elem.second;
	}
	return min;
}
double DistributionalValue::max_count() const
{
	double max = 0;
	for (auto elem : _value)
	{
		if (max <= elem.second)
			max = elem.second;
	}
	return max;
}

std::vector<double> DistributionalValue::get_mode() const
{
	std::vector<double> probs;
	for (auto elem : _value)
	{
		probs.push_back(get_mode_for(elem.second));
	}
	return probs;
}

double DistributionalValue::get_mode_for(double ai) const
{
	return (ai - 1) / (total_count() - _value.size());
}

std::vector<double> DistributionalValue::get_mean() const
{
	std::vector<double> probs;
	for (auto elem : _value)
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

//This should be avoided where ever possible.
double DistributionalValue::get_fstord_mean() const
{
	double res = 0;
	for (auto elem : _value)
	{
		//TODO: Is mode correct or should i use mean?
		DVec mof = middle_of_interval(elem.first);
		double mode = std::accumulate(mof.begin(),mof.end(),0.0)/mof.size();
		res = res + mode * get_mode_for(elem.second);
	}
	return res;
}

DVec DistributionalValue::middle_of_interval(DVKey k) const
{
	DVec res;
	for (auto interval : k) {
		if (interval.size() == 1)
			res.push_back(interval[0]);
		res.push_back((interval[0] + interval[1])/2);
	}
	return res;
}

std::vector<double> DistributionalValue::get_var() const
{
	std::vector<double> probs;
	for (auto elem : _value)
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
	return _value.total_count();
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

DVKey DistributionalValue::get_key(DVKey k) const
{
	auto it = _value.find(k);
	if (it != _value.end())
	{
		return it->first;
	}
	throw RuntimeException(TRACE_INFO, "No Key for this value.");
}



DVKeySeq DistributionalValue::get_keys() const
{
	DVKeySeq res;
	for (auto k : _value)
		res.push_back(k.first);
	return res;
}

double DistributionalValue::get_count(DVKey h) const
{
	auto pos = _value.find(h);
	if (pos != _value.end())
		return pos->second;
	throw RuntimeException(TRACE_INFO, "No Key for this value.");
}

double DistributionalValue::key_contained(DVKey ks1,DVKey ks2)
{
	Interval k1,k2;
	double sum = 1;
	for (auto zipped : boost::combine(ks1,ks2))
	{
		boost::tie(k1,k2) = zipped;
		if (k1.size() == 1 && 1 == k2.size() && k1[0] == k2[0])
			continue;
		if (k1.size() == 1 && 1 == k2.size() && k1[0] != k2[0])
			return 0;

		if (k1.size() == 1 && 2 == k2.size() && k1[0] > k2[0] && k1[0] < k2[1])
			continue;
		if (k1.size() == 1 && 2 == k2.size() && (k1[0] < k2[0] || k1[0] > k2[1]))
			return 0;

		// The part of an interval contained in a sigelton set is infinitelly small
		if (k1.size() == 2 && 1 == k2.size())
			return 0;

		//Two Intervals
		//Calculate the overlapp
		Interval res;
		if (k1[0] > k2[0])
			res.push_back(k1[0]);
		else
			res.push_back(k2[0]);

		if (k1[1] < k2[1])
			res.push_back(k1[1]);
		else
			res.push_back(k2[1]);

		if (res[0] <= res[1])
			sum *= (res[1] - res[0]) / (k1[1] - k1[0]);
		else
			return 0;
	}
	return sum;
}

double DistributionalValue::get_contained_count(DVKey h) const
{
	double res = 0;
	for (auto v : _value)
	{
		double weigth = DistributionalValue::key_contained(h,v.first);
		res += v.second * weigth;
	}
	return res;
}

double DistributionalValue::get_mode(DVKey val) const
{
	return get_mode_for(get_count(val));
}
double DistributionalValue::get_mean(DVKey val) const
{
	return get_mean_for(get_count(val));
}
double DistributionalValue::get_contained_mean(DVKey val) const
{
	return get_mean_for(get_contained_count(val));
}
double DistributionalValue::get_var(DVKey val) const
{
	return get_var_for(get_count(val));
}

std::string DistributionalValue::to_string(const std::string& indent) const
{
	std::stringstream ss;
	for (auto elem : _value)
	{
		ss << indent << "{";
		for (auto interval : elem.first)
		{
			if (interval.size() == 1)
				ss << interval[0] << ";";
			else
				ss << "["
				   << interval[0]
				   << ","
				   << interval[1]
				   << ");";
		}
		ss.seekp(-1,std::ios_base::end);
		ss << "}"
		   << " Count: "
		   << std::setprecision(18)
		   << elem.second
		   << " Mean: "
		   << get_mean_for(elem.second)
		   << std::endl;
	}
	return ss.str();
}

bool DistributionalValue::operator==(const ProtoAtom& other) const
{
	if (DISTRIBUTIONAL_VALUE != other.get_type()) return false;
	const DistributionalValue* dov = (const DistributionalValue*) &other;

	if (_value.size() != dov->_value.size()) return false;

	for (auto elem : _value) {
		double v1 = elem.second;
		double v2 = dov->_value.get(elem.first);
		if (not is_approx_eq_ulp(v1,v2))
			return false;
	}
	return true;
}

bool DistributionalValue::operator<(const ProtoAtom& other) const
{
	if (DISTRIBUTIONAL_VALUE != other.get_type())
		return DISTRIBUTIONAL_VALUE < other.get_type();

	const DistributionalValue* dov = (const DistributionalValue*) &other;

	return _value < dov->_value;
}

std::string oc_to_string(const DistributionalValuePtr dvp)
{
	std::string part = dvp->to_string("");
	return part;
}
