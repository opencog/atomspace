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
#include <sstream>

#include <opencog/atoms/distvalue/ConditionalDV.h>
#include <opencog/atoms/distvalue/DistributionalValue.h>

using namespace opencog;

ConditionalDV::ConditionalDV()
: Value(CONDITIONAL_DISTRIBUTIONAL_VALUE)
{
}

ConditionalDV::ConditionalDV(CDVrep rep)
: Value(CONDITIONAL_DISTRIBUTIONAL_VALUE)
{
	_value = rep;
}

ConditionalDV::ConditionalDV(DVKeySeq conds,std::vector<DistributionalValuePtr> dvs)
: Value(CONDITIONAL_DISTRIBUTIONAL_VALUE)
{
	auto it1 = conds.begin();
	auto it2 = dvs.begin();
	auto end1	= conds.end();
	auto end2	= dvs.end();
	if (conds.empty())
		throw RuntimeException(TRACE_INFO,"Conds may not be empty.");

	if (dvs.empty())
		throw RuntimeException(TRACE_INFO,"DVs may not be empty.");

	if (dvs.size() != conds.size())
		throw RuntimeException(TRACE_INFO,"DVs and Conds must be the same lenght.");

	for (;(it1 != end1) && (it2 != end2); ++it1, ++it2)
	{
		_value[*it1] = (*it2)->_value;
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

ConditionalDVPtr ConditionalDV::createCDV(DVKeySeq conds
                                         ,std::vector<DistributionalValuePtr> dvs)
{
	return std::make_shared<const ConditionalDV>(conds,dvs);
}

//Get all the Conditions
DVKeySeq ConditionalDV::get_conditions() const
{
	DVKeySeq res;
	for (auto gdtvpart : _value)
	{
		res.push_back(gdtvpart.first);
	}
	return res;
}

//Get all the DVs without the Conditions
std::vector<DistributionalValuePtr> ConditionalDV::get_unconditionals() const
{
	std::vector<DistributionalValuePtr> res;
	for (auto gdtvpart : _value)
	{
		res.push_back(DistributionalValue::createDV(gdtvpart.second));
	}
	return res;
}

/*
 * Merge unconditional weighted based on the overlap of the given Interval
 * to the condition interval
 */
DVCounter ConditionalDV::get_unconditionalP(DVKey h) const
{
	DVCounter res;
	for (auto elem : _value)
	{
		double weight = DistributionalValue::key_contained(h,elem.first);
		double count = elem.second.total_count();
		//DV Elems with count of 0 are not allowed
		if (count != 0 && weight != 0)
			res += elem.second * weight;
	}
	return res;
}

DistributionalValuePtr ConditionalDV::get_unconditional(DVKey k) const
{
	DVCounter res = get_unconditionalP(k);
	return DistributionalValue::createDV(res);
}

/*
 * Get a DV that is the weighted combination of all contained DVs based
 * on a Distribution of the Condition
 */
DistributionalValuePtr ConditionalDV::get_unconditional(DistributionalValuePtr condDist) const
{
	DVCounter res;
	for (auto v : condDist->_value)
	{
		double val = condDist->get_mean_for(v.second);
		DVCounter tmp = get_unconditionalP(v.first) * val;
		res += tmp;
	}
	res = res;
	return std::make_shared<const DistributionalValue>(res);
}

double ConditionalDV::total_count() const
{
	double res = 0;
	for (auto elem : _value)
	{
		res += elem.second.total_count();
	}
	return res;
}

double ConditionalDV::avg_count() const
{
	double res = 0;
	int count = 0;
	for (auto elem : _value)
	{
		count++;
		res += elem.second.total_count();
	}
	return res / count;
}


//Given a Distribution of the Condition calculate a Joint Probability distribution
DistributionalValuePtr ConditionalDV::get_joint_probability(DistributionalValuePtr base) const
{
	DVCounter res;
	DVKeySeq ivsBASE = base->get_keys();
	for (auto k1 : ivsBASE) {
		DistributionalValuePtr uncond = get_unconditional(k1);
		DVKeySeq ivsTHIS = uncond->get_keys();
		for (auto k2 : ivsTHIS) {
			DVKey k;
			k.insert(k.end(),k1.begin(),k1.end());
			k.insert(k.end(),k2.begin(),k2.end());

			//Res count based on base count
			res[k] = base->_value.at(k1) * uncond->get_mean(k2);
		}
	}
	return DistributionalValue::createDV(res);
}

// A->C + B->C => (A,B)->C
ConditionalDVPtr ConditionalDV::merge(ConditionalDVPtr cdv2) const
{
	CDVrep res;
	for (auto elem1 : _value)
	{
		for (auto elem2 : cdv2->_value)
		{
			DistributionalValuePtr dv1 = DistributionalValue::createDV(elem1.second);
			DistributionalValuePtr dv2 = DistributionalValue::createDV(elem2.second);
			DVKey k;
			k.insert(k.end(),elem1.first.begin(),elem1.first.end());
			k.insert(k.end(),elem2.first.begin(),elem2.first.end());
			res[k] = dv1->merge(dv2)->_value;
		}
	}
	return createCDV(res);
}


std::string ConditionalDV::to_string(const std::string& indent) const
{
	std::stringstream ss;
	if (_value.size() == 0)
		ss << "Empty ConditionalDV" << std::endl;
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
		ss << "} DV: "
		   << std::endl
		   << DistributionalValue(elem.second).to_string(indent + "    ")
		   << std::endl;
	}
	return ss.str();
}

bool ConditionalDV::operator==(const Value& other) const
{
	if (CONDITIONAL_DISTRIBUTIONAL_VALUE != other.get_type()) return false;
	const ConditionalDV* cov = (const ConditionalDV*) &other;

	if (_value.size() != cov->_value.size()) return false;

	for (auto elem : _value) {
		DistributionalValuePtr dv1 = DistributionalValue::createDV(elem.second);
		auto it = cov->_value.find(elem.first);
		if (it == _value.end())
			return false;
		DistributionalValuePtr dv2 = DistributionalValue::createDV(it->second);
		if (*dv1 != *dv2)
			return false;
	}
	return true;
}
