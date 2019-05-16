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
	if (_elem_count == 0)
	{
		for (unsigned int i = 0; i < x.pos.size(); i++)
		{
			lower_limits[i] = x.pos[i];
			upper_limits[i] = x.pos[i];
		}
	}
	else
	{
		for (unsigned int i = 0; i < x.pos.size(); i++)
		{
			if (x.pos[i] < lower_limits[i])
				lower_limits[i] = x.pos[i];
			if (x.pos[i] > upper_limits[i])
				upper_limits[i] = x.pos[i];
		}
	}

	if (this->_elem_count == _size)
		insertMerge(x);
	else
		CoverTree<val_t>::insert(x);
}

template <typename val_t>
void CTHist<val_t>::insertMerge(const CoverTreeNode<val_t> & x)
{
	//std::cout << "PSO: " << ::to_string(x.pos) << std::endl;
	int level;
	int parent = -1;
	int nearest_idx = findNearestNeighbor_(x,_root_idx,_root_idx,
										   _root_level,level,parent);
	CoverTreeNode<val_t> & nearest = _nodes[nearest_idx];
	//std::cout << "NEAREST: " << ::to_string(nearest.pos) << std::endl;
	if (nearest.pos == x.pos)
		nearest.value += x.value;
	else
	{
		nearest = mergeNode(x,nearest);

		std::vector<int> desc = std::vector<int>();
		descendants(nearest,desc);

		if (parent != -1 && dist(nearest,_nodes[parent]) > covdist(level+1))
		{
			_nodes[parent].children.erase(find(_nodes[parent].children.begin(),_nodes[parent].children.end(),nearest_idx));

			for (int c_idx : desc)
			{
				_nodes[c_idx].children.clear();
				insert_rec(c_idx,_nodes[parent],level+1);
			}
			_nodes[nearest_idx].children.clear();

			insert(nearest_idx,_root_idx,_root_level);
		}
		else
		{
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
		if (dist(n,_nodes[c_idx]) > covdist(level))
		{
			if (n == _nodes[_root_idx])
			{
				_nodes[c_idx].children.push_back(_root_idx);
				_root_idx = c_idx;
				p_idx = c_idx;
				_root_level++;
			}
			else
			{
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
val_t CTHist<val_t>::get_avg(DVec pos) const
{
	for (unsigned int i = 0; i < pos.size(); i++)
	{
		if (pos[i] < lower_limits[i])
			return val_t();
		if (pos[i] > upper_limits[i])
			return val_t();
	}

	CoverTreeNode<val_t> tmp = CoverTreeNode<val_t>(pos,val_t());
	double score = 0;
	const CoverTreeNode<val_t> * nearest = findNearestNeighbor(tmp);

	val_t sum1 = nearest->value * (1.0/::dist(nearest->pos,pos));
	double sum2 = 1.0/::dist(nearest->pos,pos);

	if (nearest->pos == pos)
	{
		DVec opos = DVec(pos.size());
		for (unsigned int i = 0; i < opos.size(); i++)
		{
			if (pos[i] > nearest->pos[i])
				opos[i] = upper_limits[i];
			else
				opos[i] = lower_limits[i];
		}
		if (nearest->pos == pos)
			return nearest->value;
		sum1 += val_t() * (1.0/::dist(opos,pos));
		sum2 += 1.0/::dist(opos,pos);
	}
	else
	{
		const CoverTreeNode<val_t> * opp_dir =
			findNeighborInDir(_nodes[_root_idx], pos, pos - nearest->pos,
		                      score, nearest);
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
CTHist<val_t>& CTHist<val_t>::operator+=(const CTHist<val_t>& val)
{
	*this = merge(*this,val);
	return *this;
}

template <typename val_t>
CTHist<val_t> CTHist<val_t>::remap(const DVecSeq & val) const
{
	if (val[0].size() != _dims)
		throw RuntimeException(TRACE_INFO,"Wrong number of dims to remap.");

	CTHist<val_t> res = CTHist<val_t>(val.size(),_dims);
	double sum;
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
	CTHist<val_t> res = CTHist<val_t>(_size,_dims);
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
	CTHist<val_t> res = CTHist(t1._size,t1._dims);
	for (auto elem : t1._nodes)
	{
		elem.children = std::vector<int>();
		res.insert(elem);
		//res.print();
	}

	for (auto elem : t2._nodes)
	{
		elem.children = std::vector<int>();
		res.insert(elem);
		//res.print();
	}

	return res;
}

template class CTHist<double>;
template class CTHist<CTHist<double>>;
