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

#ifndef _OPENCOG_CTHIST_H
#define _OPENCOG_CTHIST_H

#include <opencog/atoms/distvalue/CoverTree.h>

#include <boost/operators.hpp>

namespace opencog
{

template <typename val_t>
class CTHist : public CoverTree<val_t> ,
      public boost::addable<CTHist<val_t>> ,
      public boost::multiplicative<CTHist<val_t>,double>
{
	//We need access to the private default constructor in all related classes
	//we can't just make it public because it is not a valid CTHist
	friend class CoverTree<double>;
	friend class CoverTree<CTHist<double>>;
	friend class CTHist<double>;
	friend class CTHist<CTHist<double>>;

	//When inheriting from a class template we need to explicitly refere to it's
	//members either by using this->member or with using declarations as follows
	using CoverTree<val_t>::_root_idx;
	using CoverTree<val_t>::_root_level;
	using CoverTree<val_t>::_elem_count;
	using CoverTree<val_t>::_total_count;
	using CoverTree<val_t>::_dims;
	using CoverTree<val_t>::_nodes;
	using CoverTree<val_t>::findNearestNeighbor_;
	using CoverTree<val_t>::maxdist;
	using CoverTree<val_t>::covdist;
	using CoverTree<val_t>::dist;
	using CoverTree<val_t>::descendants;
	using CoverTree<val_t>::insert;
	using CoverTree<val_t>::insert_rec;

	//Maximum Number of Elements of CTHist
	int _size;
	//We store the highest and lowest value for each dimension we have ever seen
	DVec upper_limits;
	DVec lower_limits;

	/*
	 * Merge Node x with it's NearestNeighbor
	 */
	void insertMerge(const CoverTreeNode<val_t> & x);

	/*
	 * Helper for insertMerge
	 */
	void rec_move(CoverTreeNode<val_t> & n, CoverTreeNode<val_t> & x,
	              int p_idx, int level);

	/*
	 * Merge 2 Nodes int a new One
	 * Averaging postion based on count
	 * and summing count and children
	 */
	static CoverTreeNode<val_t> mergeNode(const CoverTreeNode<val_t> &,
										  const CoverTreeNode<val_t> &);

	/*
	 * Similary to findNearestNeighbor but also takes into consideration
	 * the direction
	 * score = 1/distance * max(0,cos(angle));
	 */
	const CoverTreeNode<val_t> *
		findNeighborInDir(const CoverTreeNode<val_t> & n, const DVec & t,
		                  const DVec & dir, double & score,
		                  const CoverTreeNode<val_t> * best) const;

	/*
	 * Get the averaged value for a position not in the Histogram
	 * used in remap, can't be public as it requires normalization
	 * and can return invalid val_t if it's a CTHist
	 */
	val_t get_avg(DVec pos) const;


	//Default Constructructor
	//Use with care as it doesn't creat a properly initialized CTHist
	CTHist() {}

public:

	//Same as above but they need to be public
	using CoverTree<val_t>::findNearestNeighbor;
	using CoverTree<val_t>::get;

	CTHist(int s, size_t dims)
		: CoverTree<val_t>(dims) , _size(s)
	{
		lower_limits = DVec(dims);
		upper_limits = DVec(dims);
	}

	int size() const {return _size;};

	/*
	 * Insert Node x into CTHist
	 * Taking into account max_size;
	 */
	void insert(const DVec &, const val_t &);
	void insert(const CoverTreeNode<val_t> & x);

	/*
	 * Remap the Histogram onto a different configuration of bins.
	 */
	CTHist<val_t> remap(const DVecSeq & val) const;

	/*
	 * Get the minimum and maximus count of all elements
	 */
	void getMinMaxCount(double &min, double &max) const;

	/*
	 * Mirror the Histogram around the L-ifinity mean.
	 */
	CTHist<val_t> mirrorLinf() const;

	/*
	 * Merge to CTHists into 1
	 */
	static CTHist<val_t> merge(const CTHist<val_t>&, const CTHist<val_t>&);

	/*
	 * Operator version of merge
	 */
	CTHist<val_t>& operator+=(const CTHist<val_t>&);

};

} //Namespace opencog

#endif //_OPENCOG_COVERTREE_H


