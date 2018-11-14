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

#include <opencog/atoms/proto/FloatValue.h>
#include <opencog/atoms/proto/LinkValue.h>

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

ConditionalDV::ConditionalDV(DVKeySeq conds,std::vector<DistributionalValuePtr> dvs)
: ProtoAtom(CONDITIONAL_DISTRIBUTIONAL_VALUE)
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

ConditionalDVPtr ConditionalDV::createCDV(DVKeySeq conds
										 ,std::vector<DistributionalValuePtr> dvs)
{
	return std::make_shared<const ConditionalDV>(conds,dvs);
}

DVKeySeq ConditionalDV::get_conditions() const
{
	DVKeySeq res;
	for (auto gdtvpart : value)
	{
		res.push_back(gdtvpart.first);
	}
	return res;
}

std::vector<DistributionalValuePtr> ConditionalDV::get_unconditionals() const
{
	std::vector<DistributionalValuePtr> res;
	for (auto gdtvpart : value)
	{
		res.push_back(DistributionalValue::createDV(gdtvpart.second));
	}
	return res;
}

/*
 * Merge unconditional weighted based on the distance of the given Interval
 * to the condition interval
 */
DVCounter ConditionalDV::get_unconditionalP(DVKey h) const
{
	DVCounter res;
	for (auto elem : value)
	{
		double weight = DistributionalValue::key_contained(h,elem.first);
		double count = elem.second.total_count();
		if (count != 0)
			res += elem.second / count * weight;
	}
	res = res * total_count();
	return res;
}

DistributionalValuePtr ConditionalDV::get_unconditional(DVKey k) const
{
	DVCounter res = get_unconditionalP(k);
	return DistributionalValue::createDV(res);
}

DistributionalValuePtr ConditionalDV::get_unconditional(DistributionalValuePtr condDist) const
{
	DVCounter res;
	for (auto v : condDist->value)
	{
		double val = condDist->get_mean_for(v.second);
		res += get_unconditionalP(v.first) / total_count() * val;
	}
	res = res * total_count();
	return std::make_shared<const DistributionalValue>(res);
}

double ConditionalDV::total_count() const
{
	double res = 0;
	for (auto elem : value)
	{
		res += elem.second.total_count();
	}
	return res;
}

double ConditionalDV::avg_count() const
{
	double res = 0;
	int count = 0;
	for (auto elem : value)
	{
		count++;
		res += elem.second.total_count();
	}
	return res / count;
}


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
			res[k] = base->value.at(k1) * uncond->get_mean(k2);
		}
	}
	return DistributionalValue::createDV(res);
}

// A->C + B->C => (A,B)->C
ConditionalDVPtr ConditionalDV::merge(ConditionalDVPtr cdv2) const
{
	CDVrep res;
	for (auto elem1 : value)
	{
		for (auto elem2 : cdv2->value)
		{
			DistributionalValuePtr dv1 = DistributionalValue::createDV(elem1.second);
			DistributionalValuePtr dv2 = DistributionalValue::createDV(elem2.second);
			DVKey k;
			k.insert(k.end(),elem1.first.begin(),elem1.first.end());
			k.insert(k.end(),elem2.first.begin(),elem2.first.end());
			res[k] = dv1->merge(dv2)->value;
		}
	}
	return createCDV(res);
}
/*
ConditionalDVPtr ConditionalDV::CDE(ConditionalDVPtr cdv2) const
{
	CDVrep res;
	for (auto elem : cdv2->value)
	{
		DistributionalValuePtr v1 = get_unconditional_no_match(elem.first);
		ValueCounter partres;

		double count;
		if (v1->total_count() >= elem.second.total_count())
			count = v1->total_count();
		else
			count = elem.second.total_count();

		for (auto elem2 : elem.second)
		{
			double m1 = v1->get_mean_no_match(elem2.first);
			double m2 = DistributionalValue::createDV(elem.second)->get_mean(elem2.first);
			partres[elem2.first] = count * ((m1 - m2) / (1 - m2));
		}
		res[elem.first] = partres;
	}
	return createCDV(res);
}
*/

std::string ConditionalDV::to_string(const std::string& indent) const
{
	std::stringstream ss;
	for (auto elem : value)
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

bool ConditionalDV::operator==(const ProtoAtom& rhs) const
{
	throw RuntimeException(TRACE_INFO, "Not Implemented");

	const DistributionalValue *gdtv2 = dynamic_cast<const DistributionalValue *>(&rhs);
	if (NULL == gdtv2) return false;
}

bool ConditionalDV::operator<(const ProtoAtom& other) const
{
	if (CONDITIONAL_DISTRIBUTIONAL_VALUE != other.get_type())
		return CONDITIONAL_DISTRIBUTIONAL_VALUE < other.get_type();

	const ConditionalDV* dov = (const ConditionalDV*) &other;

	return value < dov->value;
}
