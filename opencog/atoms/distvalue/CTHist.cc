/*
 * Copyright (C) 2019 SingularityNet
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

#include <algorithm>
#include <cmath>

#include <opencog/util/exceptions.h>

#include <opencog/atoms/distvalue/CTHist.h>

using namespace opencog;

template <typename val_t>
void CTHist<val_t>::insert(const DVec & pos,const val_t & value)
{
	insert(CoverTreeNode<val_t>(pos,value));
}

template <typename val_t>
void CTHist<val_t>::insert(const CoverTreeNode<val_t> & x)
{
	if (_nodes.size() == 0)
	{
		for (unsigned int i = 0; i < x.pos.size(); i++)
		{
			_lower_limits[i] = x.pos[i];
			_upper_limits[i] = x.pos[i];
		}
	}
	else
	{
		for (unsigned int i = 0; i < x.pos.size(); i++)
		{
			if (x.pos[i] < _lower_limits[i])
				_lower_limits[i] = x.pos[i];
			if (x.pos[i] > _upper_limits[i])
				_upper_limits[i] = x.pos[i];
		}
	}

	if (_nodes.size() == _max_size)
		insertMerge(x);
	else
		CoverTree<val_t>::insert(x);
}

template <typename val_t>
double CTHist<val_t>::epsilon = 0.001;

template <typename val_t>
void CTHist<val_t>::insertMerge(const CoverTreeNode<val_t> & x)
{
	int level;
	int parent = -1;
	int nearest_idx = findNearestNeighbor_(x,_root_idx,_root_idx,
										   _root_level,level,parent);
	CoverTreeNode<val_t> & nearest = _nodes[nearest_idx];
	if (dist(nearest,x) <= epsilon)
		nearest.value += x.value;
	else
	{
		nearest = mergeNode(x,nearest);

		std::vector<int> desc = std::vector<int>();
		descendants(nearest,desc);

		//If we aren't the root and we moved outside the coverdistance
		if (parent != -1 && dist(nearest,_nodes[parent]) > covdist(level+1))
		{
			//Remove our node from the parent
			_nodes[parent].children.erase(find(_nodes[parent].children.begin(),_nodes[parent].children.end(),nearest_idx));

			//Add all descendants to the parent
			for (int c_idx : desc)
			{
				_nodes[c_idx].children.clear();
				insert_rec(c_idx,_nodes[parent],level+1);
			}
			_nodes[nearest_idx].children.clear();

			//Readd our node
			insert(nearest_idx,_root_idx,_root_level);
		}
		else
		{
			//Check if the children are inside the coverdistance
			//and if not move them
			rec_move(nearest,nearest,parent,level);
		}
	}

	_total_count += get_count(x.value);
}

template <typename val_t>
void CTHist<val_t>::rec_move(CoverTreeNode<val_t> & n,
                             CoverTreeNode<val_t> & x,
                             int p_idx, int level)
{
	for (auto c_idx_it = x.children.begin(); c_idx_it != x.children.end(); )
	{
		int c_idx = *c_idx_it;
		//Child is outside the covdist we need to move it
		if (dist(n,_nodes[c_idx]) > covdist(level))
		{
			//If n is the root node the child will become the new root
			if (n == _nodes[_root_idx])
			{
				_nodes[c_idx].children.push_back(_root_idx);
				_root_idx = c_idx;
				p_idx = c_idx;
				_root_level++;
			}
			else
			{   //If we have a parent reinsert the child and it's descendants
				//on it
				std::vector<int> c_desc = std::vector<int>();
				descendants(_nodes[c_idx],c_desc);
				for (int cd_idx : c_desc)
				{
					insert_rec(cd_idx,_nodes[p_idx],level+1);
				}
				_nodes[c_idx].children.clear();
				insert_rec(c_idx,_nodes[p_idx],level+1);
			}
			c_idx_it = x.children.erase(c_idx_it);
			continue;
		}
		//Apperently the descendants don't need to follow this invariant
		//rec_move(n,_nodes[c_idx],p_idx,level);
		c_idx_it++;
	}
}

template <typename val_t>
CoverTreeNode<val_t> CTHist<val_t>::mergeNode(const CoverTreeNode<val_t> & n1,
                                              const CoverTreeNode<val_t> & n2)
{
	val_t nv = n1.value + n2.value;
	DVec np = (n1.pos * get_count(n1.value) +
	           n2.pos * get_count(n2.value)) / get_count(nv);
	CoverTreeNode<val_t> res = CoverTreeNode<val_t>(np,nv);
	res.children.insert(res.children.end(),n1.children.begin(),n1.children.end());
	res.children.insert(res.children.end(),n2.children.begin(),n2.children.end());
	return res;
}

template <typename val_t>
val_t CTHist<val_t>::get_avg(const DVec & pos) const
{
	for (unsigned int i = 0; i < pos.size(); i++)
	{
		if (pos[i] < _lower_limits[i])
			return val_t();
		if (pos[i] > _upper_limits[i])
			return val_t();
	}

	CoverTreeNode<val_t> tmp = CoverTreeNode<val_t>(pos,val_t());
	double score = 0;
	const CoverTreeNode<val_t> * nearest = findNearestNeighbor(tmp);

	if (nearest->pos == pos)
		return nearest->value;

	val_t sum1 = nearest->value * (1.0/::dist(nearest->pos,pos));
	double sum2 = 1.0/::dist(nearest->pos,pos);

	const CoverTreeNode<val_t> * opp_dir =
		findNeighborInDir(_nodes[_root_idx], pos, pos - nearest->pos,
		                  score, nearest);

	if (nearest->pos == opp_dir->pos)
	{
		DVec opos = DVec(pos.size());
		for (unsigned int i = 0; i < opos.size(); i++)
		{
			if (pos[i] > nearest->pos[i])
				opos[i] = _upper_limits[i];
			else
				opos[i] = _lower_limits[i];
		}
		if (nearest->pos == opos)
			return nearest->value;

		sum1 += val_t() * (1.0/::dist(opos,pos));
		sum2 += 1.0/::dist(opos,pos);
	}
	else
	{
		sum1 += opp_dir->value * (1.0/::dist(opp_dir->pos,pos));
		sum2 += 1.0/::dist(opp_dir->pos,pos);
	}

	return sum1 / sum2;
}

template <typename val_t>
const CoverTreeNode<val_t>*
CTHist<val_t>::findNeighborInDir(const CoverTreeNode<val_t> & n,
                                 const DVec & t, const DVec & dir, double & score,
								 const CoverTreeNode<val_t> * best) const
{
	double tmp = 1/::dist(n.pos, t) * max(0.0,angle(n.pos - t, dir));
	if (tmp > score)
	{
		score = tmp;
		best = &n;
	}

	std::vector<int> idxs(n.children.size());
	std::size_t c(0);
    std::generate(std::begin(idxs), std::end(idxs), [&]{ return c++; });

	auto cmp = [&](int i1,int i2) -> bool
				{
					auto p1 = _nodes[n.children[i1]].pos;
					auto p2 = _nodes[n.children[i2]].pos;
				   return 1/::dist(p1, t) * max(0.0,angle(p1 - t,dir)) <
					      1/::dist(p2, t) * max(0.0,angle(p2 - t,dir));
				};
	std::sort(idxs.begin(),idxs.end(),cmp);

	for (int id : idxs)
	{
		auto & q = _nodes[n.children[id]];
		double r = maxdist(q);
		double a = std::cos(min(1.0,std::acos(angle(q.pos - t,dir)) -
		                   std::acos(angleTangent(t, q.pos, r))));
		double tmp = 1/(max(0.0,::dist(q.pos, t) - r)) * max(0.0,a);

		if (tmp > score)
		    best = findNeighborInDir(q,t,dir,score,best);
	}
	return best;
}


template <typename val_t>
CTHist<val_t> CTHist<val_t>::remap(const DVecSeq & val) const
{
	if (val[0].size() != _dims)
		throw RuntimeException(TRACE_INFO,"Wrong number of dims to remap.");

	CTHist<val_t> res = CTHist<val_t>(val.size(),_dims);
	res.update_limits(_lower_limits,_upper_limits);
	double sum = 0;
	for (DVec pos : val)
	{
		val_t tmp = get_avg(pos);
		if (get_count(tmp) == 0)
			continue;
		sum += get_count(tmp) / _total_count;
		res.insert(pos,tmp);
	}
	res /= sum;
	return res;
}

template <typename val_t>
void CTHist<val_t>::update_limits(const DVec & lower, const DVec & upper)
{
	if (lower.size() != _lower_limits.size() ||
	    upper.size() != _upper_limits.size())
		throw RuntimeException(TRACE_INFO,
		                       "Can't update limits. Sizes don't macht.");

	for (unsigned int i = 0; i < lower.size(); i++)
	{
		if (lower[i] < _lower_limits[i])
			_lower_limits[i] = lower[i];
		if (upper[i] > _upper_limits[i])
			_upper_limits[i] = upper[i];
	}
}

template <typename val_t>
void CTHist<val_t>::getMinMaxCount(double &min, double &max) const
{
	min = std::numeric_limits<double>::max();
	max = 0;
	for (auto elem : _nodes)
	{
		double val = get_count(elem.value);
		if (val < min)
			min = val;
		if (val > max)
			max = val;
	}
}

template <typename val_t>
CTHist<val_t> CTHist<val_t>::mirrorLinf() const
{
	double min;
	double max;
	getMinMaxCount(min,max);
	double mms = min + max;
	CTHist<val_t> res = CTHist<val_t>(_max_size,_dims);
	for (auto elem : _nodes)
	{
		double ncount = mms - get_count(elem.value);
		update_count(elem.value, ncount);
		res.insert(elem.pos,elem.value);
	}
	return res;
}

template <typename val_t>
CTHist<val_t> CTHist<val_t>::merge(const CTHist<val_t>& t1, const CTHist<val_t>& t2)
{
	if (t1._dims != t2._dims)
		throw RuntimeException(TRACE_INFO,"Can't merge! Dimensions don't align.");

	CTHist<val_t> res = CTHist(max(t1._max_size, t2._max_size), t1._dims);
	res.update_limits(t1._lower_limits, t1._upper_limits);
	res.update_limits(t2._lower_limits, t2._upper_limits);

	for (auto elem : t1._nodes)
	{
		elem.children = std::vector<int>();
		res.insert(elem);
	}

	for (auto elem : t2._nodes)
	{
		elem.children = std::vector<int>();
		res.insert(elem);
	}

	return res;
}

template <typename val_t>
void CTHist<val_t>::merge(const CTHist<val_t>& other)
{
	if (_dims != other._dims)
		throw RuntimeException(TRACE_INFO,"Can't merge! Dimensions don't align.");

	update_limits(other._lower_limits,other._upper_limits);
	for (auto elem : other._nodes)
	{
		elem.children = std::vector<int>();
		insert(elem);
	}
}

template <typename val_t>
CTHist<val_t> CTHist<val_t>::join(const CTHist<val_t>& t1, const CTHist<val_t>& t2)
{
	CTHist<val_t> res = CTHist(t1._max_size * t2._max_size,t1._dims + t2._dims);
	DVec lower = t1._lower_limits;
	DVec upper = t1._upper_limits;
	lower.insert(lower.end(),t2._lower_limits.begin(),t2._lower_limits.end());
	upper.insert(upper.end(),t2._upper_limits.begin(),t2._upper_limits.end());
	res.update_limits(lower,upper);
	for (auto elem1 : t1._nodes)
	{
		for (auto elem2 : t2._nodes)
		{
			DVec k;
			k.insert(k.end(),elem1.pos.begin(),elem1.pos.end());
			k.insert(k.end(),elem2.pos.begin(),elem2.pos.end());
			val_t tmp = elem1.value + elem2.value;
			res.insert(k,tmp);
		}
	}
	res *=  (t1.total_count() + t2.total_count()) / res.total_count();
	return res;
}

template <typename val_t>
CTHist<val_t>& CTHist<val_t>::operator+=(const CTHist<val_t>& val)
{
	merge(val);
	return *this;
}

template class CTHist<double>;
template class CTHist<CTHist<double>>;
