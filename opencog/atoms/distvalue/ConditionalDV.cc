/*
 * opencog/atoms/distvalue/ConditionalDV.cc
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
	: Value(CONDITIONAL_DISTRIBUTIONAL_VALUE) , _value(0,0) {}

ConditionalDV::ConditionalDV(const CDVrep &rep)
	: Value(CONDITIONAL_DISTRIBUTIONAL_VALUE) , _value(rep) {}

ConditionalDV::ConditionalDV(const DVecSeq &conds,
                             const std::vector<DistributionalValuePtr> &dvs)
	: Value(CONDITIONAL_DISTRIBUTIONAL_VALUE) , _value(conds.size(),conds[0].size())
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
		_value.insert(*it1,(*it2)->_value);
	}
}

ConditionalDVPtr ConditionalDV::createCDV()
{
	return std::make_shared<const ConditionalDV>();
}

ConditionalDVPtr ConditionalDV::createCDV(const CDVrep &rep)
{
	return std::make_shared<const ConditionalDV>(rep);
}

ConditionalDVPtr ConditionalDV::createCDV(const DVecSeq &conds,
                                          const std::vector<DistributionalValuePtr> &dvs)
{
	return std::make_shared<const ConditionalDV>(conds,dvs);
}

//Get all the DVs without the Conditions
std::vector<DistributionalValuePtr> ConditionalDV::get_unconditionals() const
{
	std::vector<DistributionalValuePtr> res;
	for (auto node : _value)
	{
		res.push_back(DistributionalValue::createDV(node.value));
	}
	return res;
}



/*
 * Get a DV that is the weighted combination of all contained DVs based
 * on a Distribution of the Condition
 */
DistributionalValuePtr ConditionalDV::get_unconditional(DistributionalValuePtr condDist) const
{
	auto max_size = _value[0].value.max_size();
	auto dims = _value[0].value.dims();
	CTHist<double> res = CTHist<double>(max_size,dims);
	double sum = 0;

	DVecSeq keys = condDist->_value.get_posvec();
	CDVrep remaped = _value.remap(keys);

	for (auto v : remaped)
	{
		double val = condDist->get_mean(v.pos);
		double vcount = get_count(v.value);
		double norm = condDist->get_count(v.pos) / vcount;
		res += (v.value * norm) * val;
	}
	return std::make_shared<const DistributionalValue>(res);
}

//Given a Distribution of the Condition calculate a Joint Probability distribution
DistributionalValuePtr ConditionalDV::get_joint_probability(DistributionalValuePtr base) const
{
	size_t s1 = base->_value.max_size();
	size_t d1 = base->_value.dims();
	size_t s2 = _value.begin()->value.max_size();
	size_t d2 = _value.begin()->value.dims();
	CTHist<double> res = CTHist<double>(s1*s2,d1+d2);
	DVecSeq ivsBASE = base->_value.get_posvec();

	ConditionalDVPtr remaped = remap(ivsBASE);

	DVec lower = base->_value.lower_limits();
	DVec upper = base->_value.upper_limits();
	for (unsigned int i = 0; i < lower.size(); i++)
	{
		if (_value.lower_limits()[i] < lower[i])
			lower[i] = _value.lower_limits()[i];
		if (_value.upper_limits()[i] > upper[i])
			upper[i] = _value.upper_limits()[i];
	}

	DVec lower2 = _value[0].value.lower_limits();
	DVec upper2 = _value[0].value.upper_limits();

	for (DVec k1 : remaped->value().get_posvec())
	{
		DistributionalValuePtr uncond =
			DistributionalValue::createDV(remaped->value().get(k1));
		DVecSeq ivsTHIS = uncond->_value.get_posvec();

		for (unsigned int i = 0; i < lower2.size(); i++)
		{
			if (uncond->_value.lower_limits()[i] < lower2[i])
				lower2[i] = uncond->_value.lower_limits()[i];
			if (uncond->_value.upper_limits()[i] > upper2[i])
				upper2[i] = uncond->_value.upper_limits()[i];
		}

		for (DVec k2 : ivsTHIS)
		{
			DVec k;
			k.insert(k.end(),k1.begin(),k1.end());
			k.insert(k.end(),k2.begin(),k2.end());

			//Res count based on base count
			double v1 = base->_value.get(k1);
			double v2 = uncond->get_mean(k2);
			res.insert(k,v1 * v2);
		}
	}
	lower.insert(lower.end(),lower2.begin(),lower2.end());
	upper.insert(upper.end(),upper2.begin(),upper2.end());
	res.update_limits(lower,upper);
	return DistributionalValue::createDV(res);
}

double ConditionalDV::total_count() const
{
	return _value.total_count();
}

double ConditionalDV::avg_count() const
{
	double res = 0;
	int count = 0;
	for (auto elem : _value)
	{
		count++;
		res += elem.value.total_count();
	}
	return res / count;
}

double ConditionalDV::get_confidence() const
{
	int c = total_count();
	return DistributionalValue::to_conf(c);
}

// A->C + A->C => A->C
ConditionalDVPtr ConditionalDV::merge(ConditionalDVPtr other) const
{
	CDVrep hist = CDVrep::merge(_value,other->_value);
	return createCDV(hist);
}

// A->C + B->C => (A,B)->C
ConditionalDVPtr ConditionalDV::join(ConditionalDVPtr other) const
{
	CDVrep hist = CDVrep::join(_value,other->_value);
	return createCDV(hist);
}

ConditionalDVPtr ConditionalDV::remap(const DVecSeq &k) const
{
	CDVrep rep = _value.remap(k);
	return createCDV(rep);
}

//Get all the Conditions
DVecSeq ConditionalDV::get_conditions() const
{
	return _value.get_posvec();
}

bool ConditionalDV::operator==(const Value& other) const
{
	if (CONDITIONAL_DISTRIBUTIONAL_VALUE != other.get_type()) return false;
	const ConditionalDV* cov = (const ConditionalDV*) &other;

	if (_value != cov->_value)
		return false;
	return true;
}

std::string ConditionalDV::to_string(const std::string& indent) const
{
	std::stringstream ss;
	if (_value.elem_count() == 0)
		ss << "Empty ConditionalDV" << std::endl;

	ss << "ConditionalDV:\n";
	for (auto elem : _value)
	{
		ss << indent << elem.pos;
		ss << std::endl
		   << DistributionalValue(elem.value).to_string(indent + "    ")
		   << std::endl;
	}
	return ss.str();
}

