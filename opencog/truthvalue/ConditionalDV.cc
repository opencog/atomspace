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

ConditionalDV::ConditionalDV(ProtomSeq conds,std::vector<DistributionalValuePtr> dvs)
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

ConditionalDVPtr ConditionalDV::createCDV(ProtomSeq conds
										 ,std::vector<DistributionalValuePtr> dvs)
{
	return std::make_shared<const ConditionalDV>(conds,dvs);
}

ProtomSeq ConditionalDV::get_conditions() const
{
	ProtomSeq res;
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
		res.push_back(std::make_shared<const DistributionalValue>(gdtvpart.second));
	}
	return res;
}

DistributionalValuePtr ConditionalDV::get_unconditional(ProtoAtomPtr h) const
{
	return std::make_shared<const DistributionalValue>(value.find(h)->second);
}

DistributionalValuePtr ConditionalDV::get_unconditional(DistributionalValuePtr condDist) const
{
	ValueCounter res;
	for (auto gdtvpart : value)
	{
		double val = condDist->get_mean(gdtvpart.first);
		double count = gdtvpart.second.total_count();
		if (count != 0)
			res += (gdtvpart.second / count) * val;
	}
	res = res * condDist->total_count();
	return DistributionalValue::createDV(res);
}

/*
 * Merge unconditional weighted based on the distance of the given Interval
 * to the condition interval
 * TODO: Only use closest conditions? Seperate function?
 */
ValueCounter ConditionalDV::get_unconditional_no_matchP(ProtoAtomPtr h) const
{
	ValueCounter res;
	for (auto gdtvpart : value)
	{
		double dist = DistributionalValue::interval_dist(gdtvpart.first,h);
		double count = gdtvpart.second.total_count();
		if (count != 0) {
			res += gdtvpart.second / count * (1 - dist);
		}
	}
	res = res * total_count();
	return res;
}

DistributionalValuePtr ConditionalDV::get_unconditional_no_match(ProtoAtomPtr h) const
{
	ValueCounter res = get_unconditional_no_matchP(h);
	return std::make_shared<const DistributionalValue>(res);
}

DistributionalValuePtr ConditionalDV::get_unconditional_no_match(DistributionalValuePtr condDist) const
{
	ValueCounter res;
	for (auto v : condDist->value)
	{
		double val = condDist->get_mean(v.first);
		res += get_unconditional_no_matchP(v.first) / total_count() * val;
	}
	res = res * total_count();
	return std::make_shared<const DistributionalValue>(res);
}

double ConditionalDV::total_count() const
{
	double res = 0;
	for (auto gdtvpart : value)
	{
		res += gdtvpart.second.total_count();
	}
	return res;
}

double ConditionalDV::avg_count() const
{
	double res = 0;
	int count = 0;
	for (auto gdtvpart : value)
	{
		count++;
		res += gdtvpart.second.total_count();
	}
	return res / count;
}


DistributionalValuePtr ConditionalDV::get_joint_probability(DistributionalValuePtr base) const
{
	ValueCounter res;
	ProtomSeq ivsBASE = base->get_keys();
	for (auto h1 : ivsBASE) {
		DistributionalValuePtr uncond = get_unconditional_no_match(h1);
		ProtomSeq ivsTHIS = uncond->get_keys();
		for (auto h2 : ivsTHIS) {
			ProtomSeq linkelems;
			if (h1->is_type(LINK_VALUE))
			{
				ProtomSeq hs = LinkValueCast(h1)->value();
				linkelems.insert(linkelems.end(),hs.begin(),hs.end());
			} else {
				linkelems.push_back(h1);
			}
			if (h2->is_type(LINK_VALUE))
			{
				ProtomSeq hs = LinkValueCast(h2)->value();
				linkelems.insert(linkelems.end(),hs.begin(),hs.end());
			} else {
				linkelems.push_back(h2);
			}

			ProtoAtomPtr link = createLinkValue(linkelems);

			//Res count based on base count
			//maybe should be based on base + this /2 or min of the two.
			res[link] = base->value.at(h1) * uncond->get_mean(h2);
		}
	}
	return std::make_shared<const DistributionalValue>(res);
}

ConditionalDVPtr ConditionalDV::merge(ConditionalDVPtr cdv2) const
{
	CDVrep res;
	for (auto elem1 : value)
	{
		for (auto elem2 : cdv2->value)
		{
			DistributionalValuePtr dv1 = DistributionalValue::createDV(elem1.second);
			DistributionalValuePtr dv2 = DistributionalValue::createDV(elem2.second);
			ProtomSeq seq;
			seq.push_back(elem1.first);
			seq.push_back(elem2.first);
			ProtoAtomPtr link = createLinkValue(seq);
			res[link] = dv1->merge(dv2)->value;
		}
	}
	return createCDV(res);
}

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

bool ConditionalDV::operator<(const ProtoAtom& other) const
{
	if (CONDITIONAL_DISTRIBUTIONAL_VALUE != other.get_type())
		return CONDITIONAL_DISTRIBUTIONAL_VALUE < other.get_type();

	const ConditionalDV* dov = (const ConditionalDV*) &other;

	return value < dov->value;
}
