/*
 * opencog/truthvalue/DVFormulas.cc
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

#include <opencog/truthvalue/DVFormulas.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

DVec DVFormulas::get_key_min(DVKey k)
{
	std::vector<double> res;
	for (auto interval : k)
		res.push_back(interval[0]);
	return res;
}

DVec DVFormulas::get_key_max(DVKey k)
{
	std::vector<double> res;
	for (auto interval : k)
		if (interval.size() == 1)
			res.push_back(interval[0]);
		else
			res.push_back(interval[1]);

	return res;
}
ConditionalDVPtr DVFormulas::joint_to_cdv(DistributionalValuePtr dv1
									     ,DistributionalValuePtr dv2
										 ,int idx)
{
	CDVrep res;
	size_t dv1dims = dv1->value().begin()->first.size();
	size_t dv2dims = dv2->value().begin()->first.size();
	if (dv1dims <= 1)
		throw RuntimeException(TRACE_INFO,"Can't divide non Joint DV.");
	if (dv1dims - 1 != dv2dims)
		throw RuntimeException(TRACE_INFO,"The Divisor DV has to have exaclty 1 less dimensions then then dividend. This is not the case.");

	for (auto elem : dv1->value())
	{
		DVKey hs = elem.first;

		DVKey h = DVKey{hs[idx]};
		hs.erase(hs.begin() + idx);

		if (dv2->get_contained_mean(hs) != 0)
			res[hs][h] = dv1->get_mean(elem.first)
					   / dv2->get_contained_mean(hs)
					   * dv2->total_count();
	}
	return ConditionalDV::createCDV(res);
}

DistributionalValuePtr DVFormulas::sum_joint(DistributionalValuePtr dv,int pos)
{
	DVCounter res;
	for (auto elem : dv->value())
	{
		DVKey key = elem.first;
		key.erase(key.begin() + pos);
		res[key] += elem.second;
	}
	return DistributionalValue::createDV(res);
}

bool DVFormulas::comperator(Elem elem1,Elem elem2)
{
	DVec v1 = get_key_min(elem1.first);
	DVec v2 = get_key_min(elem2.first);
	return compare(v1,v2);
}

bool DVFormulas::compare(DVec v1,DVec v2)
{
	auto i1 = v1.begin();
	auto i2 = v2.begin();
	for (; i1 != v1.end() && i2 != v2.end();)
	{
		if (i1 != i2)
			return i1 < i2;
		i1++;
		i2++;
	}
	return false;
};


//Create a Conjuction from 2 DVs
DistributionalValuePtr
DVFormulas::conjuction(DistributionalValuePtr dv1
		              ,DistributionalValuePtr dv2)
{
	std::set<Elem,Comparator> set1(dv1->value().begin(),dv1->value().end(),comperator);
	std::set<Elem,Comparator> set2(dv2->value().begin(),dv2->value().end(),comperator);

	DVCounter res;
	double count = std::min(dv1->total_count(),dv2->total_count());

	auto it1 = set1.begin();
	auto it2 = set2.begin();

	double m1 = 1;
	double m2 = 1;

	while (m1 != 0 && m2 != 0)
	{
		DVec v1 = get_key_min(it1->first);
		DVec v2 = get_key_min(it2->first);
		if (compare(v1 , v2))
		{
			double mean = dv1->get_mean_for(it1->second);
			res[it1->first] += count * mean * m2;
			m1 -= mean;
			it1++;
		}
		else
		{
			double mean = dv1->get_mean_for(it2->second);
			res[it2->first] += count * mean * m1;
			m2 -= mean;
			it2++;
		}
	}

	return DistributionalValue::createDV(res);
}

//Create a disjuction from 2 DVs
DistributionalValuePtr
DVFormulas::disjuction(DistributionalValuePtr dv1
		              ,DistributionalValuePtr dv2)
{
	std::set<Elem,Comparator> set1(dv1->value().begin(),dv1->value().end(),comperator);
	std::set<Elem,Comparator> set2(dv2->value().begin(),dv2->value().end(),comperator);

	DVCounter res;
	double count = std::min(dv1->total_count(),dv2->total_count());

	auto it1 = set1.begin();
	auto it2 = set2.begin();

	double m1 = 1;
	double m2 = 1;

	while (m1 != 0 && m2 != 0)
	{
		DVec v1 = get_key_max(it1->first);
		DVec v2 = get_key_max(it2->first);
		if (compare(v1 , v2))
		{
			double mean = dv1->get_mean_for(it1->second);
			res[it1->first] += count * mean * m2;
			m1 -= mean;
			it1++;
		}
		else
		{
			double mean = dv1->get_mean_for(it2->second);
			res[it2->first] += count * mean * m1;
			m2 -= mean;
			it2++;
		}
	}

	return DistributionalValue::createDV(res);
}
