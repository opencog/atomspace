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

#include <vector>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <cmath>

#include <opencog/util/exceptions.h>

#include <opencog/atoms/distvalue/CoverTree.h>

using namespace std;
using namespace opencog;

//CoverTreeNode

template <typename val_t>
bool CoverTreeNode<val_t>::operator!=(const CoverTreeNode<val_t>& other) const
{
	return !(*this == other);
}

template <typename val_t>
bool CoverTreeNode<val_t>::operator==(const CoverTreeNode<val_t>& other) const
{
	if (pos != other.pos)
		return false;

	if (value != other.value)
		return false;

	for (auto elem : children)
	{
		auto res = std::find(other.children.begin(),other.children.end(), elem);

		if (res == other.children.end())
			return false;

		if (elem != *res)
			return false;
	}
	return true;
}

//CoverTree

template <typename val_t>
CoverTree<val_t>::CoverTree()
	: _root_idx(-1) , _root_level(0) , _elem_count(0) ,
	_total_count(0) , _dims(1) {}

template <typename val_t>
CoverTree<val_t>::CoverTree(int dims)
	:  _root_idx(-1) , _root_level(0) , _elem_count(0) ,
	_total_count(0) , _dims(dims) {}

template <typename val_t>
CoverTree<val_t>::CoverTree(CoverTreeNode<val_t> n,int dims)
	:  _root_idx(-1) , _root_level(0) , _elem_count(1) , _dims(dims) {
	_nodes.push_back(n);
	_root_idx = _nodes.size() - 1;
    _total_count = get_count(n.value);
}

template <typename val_t>
double CoverTree<val_t>::dist(const CoverTreeNode<val_t> & n1,
                              const CoverTreeNode<val_t> & n2)
{
	return ::dist(n1.pos,n2.pos);
}

template <typename val_t>
double CoverTree<val_t>::covdist(int level)
{
	return pow(2,level);
}

//TODO: Cache This?
template <typename val_t>
double CoverTree<val_t>::maxdist(const CoverTreeNode<val_t> & n) const
{
	double max = 0;
	for (int c : n.children)
	{
		double tmp = dist(n,_nodes[c]);
		if (tmp > max)
			max = tmp;
	}
	return max;
}

template <typename val_t>
int CoverTree<val_t>::popLeaf(CoverTreeNode<val_t> & n)
{
	int c_id = n.children.back();
	CoverTreeNode<val_t> & c = _nodes[c_id];
	if (c.children.size() == 0)
	{
		n.children.pop_back();
		return c_id;
	}
	return popLeaf(c);
}

template <typename val_t>
const CoverTreeNode<val_t> *
CoverTree<val_t>::findNearestNeighbor(const DVec & pos) const
{
	const CoverTreeNode<val_t> & x = CoverTreeNode<val_t>(pos,val_t());
	return findNearestNeighbor_(x,_nodes[_root_idx],&_nodes[_root_idx]);
}

template <typename val_t>
const CoverTreeNode<val_t> *
CoverTree<val_t>::findNearestNeighbor(const CoverTreeNode<val_t> & x) const
{
	return findNearestNeighbor_(x,_nodes[_root_idx],&_nodes[_root_idx]);
}

template <typename val_t>
const CoverTreeNode<val_t> *
CoverTree<val_t>::findNearestNeighbor_(const CoverTreeNode<val_t> & x,
									   const CoverTreeNode<val_t> & p,
									   const CoverTreeNode<val_t> * y) const
{
	if (dist(x,p) < dist(x,*y))
		y = &p;

	std::vector<int> idxs(p.children.size());
	std::size_t n(0);
    std::generate(std::begin(idxs), std::end(idxs), [&]{ return n++; });

	auto cmp = [&](int i1,int i2) -> bool
				{
				   return dist(_nodes[p.children[i1]],x) <
					      dist(_nodes[p.children[i2]],x);
				};
	std::sort(idxs.begin(),idxs.end(),cmp);

	for (int it : idxs)
	{
		const CoverTreeNode<val_t> & tmp = _nodes[p.children[it]];
		if (dist(x,*y) > dist(x,tmp) - maxdist(tmp))
		{
			y = findNearestNeighbor_(x,tmp,y);
		}
	}
	return y;
}

template <typename val_t>
int CoverTree<val_t>::findNearestNeighbor_(const CoverTreeNode<val_t> & x,
										   int curr_idx, int best_idx, int level,
										   int & ret_level, int & parent)
{
	CoverTreeNode<val_t> & p = _nodes[curr_idx];
	CoverTreeNode<val_t> * y = &_nodes[best_idx];
	if (dist(x,p) < dist(x,*y))
	{
		best_idx = curr_idx;
		y = &_nodes[best_idx];
		ret_level = level;
	}

	std::vector<int> idxs(p.children.size());
	std::size_t n(0);
    std::generate(std::begin(idxs), std::end(idxs), [&]{ return n++; });

	auto cmp = [&](int i1,int i2) -> bool
				{
				   return dist(_nodes[p.children[i1]],x) <
					      dist(_nodes[p.children[i2]],x);
				};
	std::sort(idxs.begin(),idxs.end(),cmp);

	for (int it : idxs)
	{
		int c_idx = p.children[it];
		CoverTreeNode<val_t> & c = _nodes[c_idx];
		if (dist(x,*y) > dist(x,c) - maxdist(c))
		{
			best_idx = findNearestNeighbor_(x,c_idx,best_idx,
											level - 1,ret_level,parent);
			y = &_nodes[best_idx];
			if (*y == c)
				parent = curr_idx;
		}
	}
	return best_idx;
}

template <typename val_t>
void CoverTree<val_t>::insert(const DVec & pos, const val_t & value)
{
	insert(CoverTreeNode<val_t>(pos,value));
}

template <typename val_t>
void CoverTree<val_t>::insert(const CoverTreeNode<val_t> & x)
{
	if (x.pos.size() != _dims)
		throw RuntimeException(TRACE_INFO,"Wrong Number of Dimensions.");

	_total_count += get_count(x.value);

	if (_elem_count == 0)
	{
		_nodes.push_back(x);
		_root_idx = _nodes.size() - 1;
		_elem_count = 1;
		return;
	}

	if (dist(x,_nodes[_root_idx]) == 0)
	{
		_nodes[_root_idx].value += x.value;
		return;
	}

	if (_nodes[_root_idx].children.size() == 0)
	{
		double d = dist(x,_nodes[_root_idx]);
		if (d == 0)
		{
			_nodes[_root_idx].value += x.value;
			return;
		}
		_root_level = ceilf(log2(d));
		_nodes.push_back(x);
		_nodes[_root_idx].children.push_back(_nodes.size() - 1);
		_elem_count++;
		return;
	}

	int node_idx = _nodes.size();
	_nodes.push_back(x);
	insert(node_idx,_root_idx,_root_level);
	_elem_count++;
}

template <typename val_t>
void CoverTree<val_t>::insert(int node_idx, int & p_id, int level)
{
	CoverTreeNode<val_t> & x = _nodes[node_idx];
	CoverTreeNode<val_t> p = _nodes[p_id];

	if (dist(x,p) > covdist(level))
	{
		//std::cout << "Outside current covdist: " << x[0] << "\n";
		while (dist(x,p) > 2*covdist(level))
		{
			if (p.children.size() != 0)
			{
				int c_id = popLeaf(_nodes[p_id]);
				_nodes[c_id].children.push_back(p_id);
				p_id = c_id;
				p = _nodes[p_id];
			}
			level++;
		}
		level++;
		x.children.push_back(p_id);
		p_id = node_idx;
		if (p_id == _root_idx)
			_root_level = level;
		return;
	}
	insert_rec(node_idx,_nodes[p_id],level);
}


template <typename val_t>
void CoverTree<val_t>::insert_rec(int node_idx,
                                  CoverTreeNode<val_t> & p,
                                  int level)
{
	const CoverTreeNode<val_t> & x = _nodes[node_idx];
	if (dist(x,p) == 0)
	{
		p.value += x.value;
		return;
	}

	for (int q : p.children)
	{
		if (dist(x,_nodes[q]) < covdist(level-1))
		{
			insert_rec(node_idx,_nodes[q],level-1);
			return;
		}
	}
	p.children.push_back(node_idx);
}

template <typename val_t>
val_t CoverTree<val_t>::get(const DVec & pos) const
{
	CoverTreeNode<val_t> tmp = CoverTreeNode<val_t>(pos,val_t());
	const CoverTreeNode<val_t> * nearest =
		findNearestNeighbor_(tmp,_nodes[_root_idx],&_nodes[_root_idx]);
	if (nearest->pos == pos)
		return nearest->value;
	else
		throw RuntimeException(TRACE_INFO,"No node with given postion in the Histogram.");
}

template <typename val_t>
DVecSeq CoverTree<val_t>::get_posvec() const
{
	DVecSeq res(_nodes.size());
	auto it = res.begin();
	for (auto elem : _nodes)
	{
		*it = elem.pos;
		it++;
	}
	return res;
}

template <typename val_t>
void CoverTree<val_t>::descendants(const CoverTreeNode<val_t> & p,
                                   std::vector<int> & res) const
{
	res.insert(res.end(),p.children.begin(),p.children.end());
	for (int q : p.children)
	{
		descendants(_nodes[q],res);
	}
}

template <typename val_t>
CoverTree<val_t> CoverTree<val_t>::merge(const CoverTree<val_t>& t1,
                                         const CoverTree<val_t>& t2)
{
	CoverTree<val_t> res = CoverTree(t1._dims);
	for (auto elem : t1._nodes)
	{
		res.insert(elem);
	}
	for (auto elem : t2._nodes)
	{
		res.insert(elem);
	}
	return res;
}

template <typename val_t>
CoverTree<val_t>& CoverTree<val_t>::operator+=(const CoverTree<val_t>& val)
{
	*this = merge(*this,val);
	return *this;
}

template <typename val_t>
bool CoverTree<val_t>::is_valid() const
{
	return is_valid_rec(_root_idx,_root_level);
}

template <typename val_t>
bool CoverTree<val_t>::is_valid_rec(int idx,int level) const
{
	for (auto c_idx : _nodes[idx].children)
	{
		if (dist(_nodes[c_idx],_nodes[idx]) > covdist(level))
			return false;
		if (!is_valid_rec(c_idx,level-1))
			return false;
	}
	return true;
}

template <typename val_t>
CoverTree<val_t>& CoverTree<val_t>::operator*=(const double& val)
{
	for (auto & elem : _nodes)
	{
		elem.value *= val;
	}
	_total_count *= val;
	return *this;
}
template <typename val_t>
CoverTree<val_t>& CoverTree<val_t>::operator/=(const double& val)
{
	for (auto & elem : _nodes)
	{
		elem.value /= val;
	}
	_total_count /= val;
	return *this;
}

template <typename val_t>
bool CoverTree<val_t>::operator!=(const CoverTree<val_t>& other) const
{
	return !(*this == other);
}

template <typename val_t>
bool CoverTree<val_t>::operator==(const CoverTree<val_t>& other) const
{
	if (_elem_count != other._elem_count)
		return false;

	if (_dims != other._dims)
		return false;

	for (auto elem : _nodes)
	{
		try
		{
			val_t oval = other.get(elem.pos);
			if (oval != elem.value)
				return false;
		}
		catch (const RuntimeException & e)
		{
			return false;
		}

	}
	return true;
}

template <typename val_t>
std::string CoverTree<val_t>::to_string() const
{
	if (_root_idx == -1)
		return "This CoverTree is Empty!\n";

	std::stringstream ss;
	int level = _root_level;
	std::vector<int> nodes = std::vector<int>{_root_idx};
	std::vector<int> next;

	ss << "Elem_Count: " << _elem_count << std::endl;
	ss << "Total: " << _total_count << std::endl;
	while (true)
	{
		ss << "Level: " << level << std::endl;
		for (int n_id : nodes)
		{
			auto & n = _nodes[n_id];
			ss << n.pos << ": " << n.value << std::endl;
			next.insert(next.end(),n.children.begin(),n.children.end());
		}

		if (next.size() == 0)
			return ss.str();

		nodes = next;
		next = std::vector<int>{};
		level = level - 1;
	}
	return ss.str();
}

template <typename val_t>
void CoverTree<val_t>::print() const
{
	std::cout << to_string() << std::flush;
}


double opencog::get_count(const double v)
{
	return v;
}

double opencog::get_count(const CoverTree<double>& ct)
{
	return ct.total_count();
}

void opencog::update_count(double & v,const double n)
{
	v = n;
}

void opencog::update_count(CoverTree<double>& ct,const double n)
{
	double tmp = n / ct.total_count();
	ct *= tmp;
}

#include <opencog/atoms/distvalue/CTHist.h>

template class CoverTree<double>;
template class CoverTree<CoverTree<double>>;
template class CoverTree<CTHist<double>>;
