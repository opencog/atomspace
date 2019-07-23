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


#ifndef _OPENCOG_COVERTREE_H
#define _OPENCOG_COVERTREE_H

#include <ostream>

#include <boost/operators.hpp>

#include <opencog/atoms/distvalue/DVecUtils.h>

using namespace std;

namespace opencog
{


/*
 * The Node Type used in a CoverTree
 * containing its postion and value
 * aswell as the indices of it's children
 *
 * Does not contain the level at which the node resides as this can be calculated
 * easily on the fly.
 *
 * TODO: storing the maxdist in the node might speed up certain Operations
 *
 * the value needs to be countable i.e. implemente the following
 * double get_count(val_t val) and void update_count(val_t & val,double new_val)
*/
template <typename val_t>
class CoverTreeNode : boost::addable<CoverTreeNode<val_t>>
{
public:
	CoverTreeNode() {}
	CoverTreeNode(DVec x, val_t v) : pos(x) , value(v) {}
	DVec pos;
	val_t value;

	std::vector<int> children;

	bool operator!=(const CoverTreeNode<val_t> & other) const;
	bool operator==(const CoverTreeNode<val_t> & other) const;
};


template <typename val_t>
using CTNSeq = std::vector<CoverTreeNode<val_t>>;

/*
 * The CoverTree Class
 */
template <typename val_t>
class CoverTree
{
protected:
	int _root_idx;
	int _root_level;
	//Sum of the counts of all Nodes in the Tree
	double _total_count;
	//Number of Dimensions of this Tree
	size_t _dims;
	//All Nodes stored in the Tree
	std::vector<CoverTreeNode<val_t>> _nodes;

	/*
	 * Calculate the Distance between 2 nodes
	 */
	static double dist(const CoverTreeNode<val_t> & n1,
	                   const CoverTreeNode<val_t> & n2);

	/*
	 * Calculate the cover distance for a given level
	 */
	static double covdist(int level);

	/*
	 * Get the Maximumdistance of a node to it's children
	 */
	double maxdist(const CoverTreeNode<val_t> & n) const;

	/*
	 * Return the index of a Leaf Node and Remove it as a child from it's parent
	 * You need to reinsert this node as a child or root node.
	 */
	int popLeaf(CoverTreeNode<val_t> & n);

	/*
	 * This Helper Implements the actual algorithm for finding the
	 * NearestNeighbor
	 */
	const CoverTreeNode<val_t>*
		findNearestNeighbor_(const CoverTreeNode<val_t> & x,
			      			 const CoverTreeNode<val_t> & p,
							 const CoverTreeNode<val_t> * y) const;

	/*
	 * Internal Helper that returns the index of the node and some additional
	 * context such as it's level and it's parents index.
	 */
	int findNearestNeighbor_(const CoverTreeNode<val_t> & x, int p, int y,
							 int level, int & ret_level, int & parent);

	/*
	 * Insert a Node x into the Node p at level:level;
	 */
	void insert(int node_idx, int & p_id, int level);

	/*
	 * Insert x Recursivly into p at level: level;
	 */
	void insert_rec(int node_idx, CoverTreeNode<val_t> & p, int level);

	/*
	 * Check that the Tree is Valid starting from Node at idx and the gvien Level
	 */
    bool is_valid_rec(int idx, int level) const;

public:

	CoverTree();
	CoverTree(int dims);
	CoverTree(CoverTreeNode<val_t> n, int dims);

	//Some Getters for Intersing Variables
	int elem_count() const {return _nodes.size();};
	size_t dims() const {return _dims;};
	double total_count() const {return _total_count;};

	/*
	 * Insert a Node into the Tree
	 * Public Interface deals with special cases if there no or only 1 node
	 * in the Tree
	 */
	void insert(const DVec&, const val_t&);
	void insert(const CoverTreeNode<val_t> & x);

	/*
	 * Get the value at a given Position
	 * throws Expection if there is no Node at the given Postion
	 */
	val_t get(const DVec & pos) const;
	val_t get(const DVec & pos,val_t def) const;

	/*
	 * Get a Vector of all positions
	 */
	DVecSeq get_posvec() const;

	/*
	 * Returns a list of all Descendants
	 */
	void descendants(const CoverTreeNode<val_t> &, std::vector<int> & res) const;

	/*
	 * This Function is the public Interface to find the NearestNeighbor
	 */
	const CoverTreeNode<val_t>* findNearestNeighbor(const DVec & pos) const;
	const CoverTreeNode<val_t>* findNearestNeighbor(const CoverTreeNode<val_t> & x) const;

	/*
	 * Merges 2 Trees together into a new one
	 *
	 * XXX This is a very simple version that just inserts all of the nodes
	 * into a new Tree, Upgrade to the alogrithm in "Faster Cover Trees"
	 */
	static CoverTree<val_t> merge(const CoverTree<val_t>&, const CoverTree<val_t>&);

	/*
	 * Operator version of merge
	 */
	CoverTree<val_t>& operator+=(const CoverTree<val_t>&);

	/*
	 * Check if the Tree is still valid. Public Interface
	 */
	bool is_valid() const;

	/*
	 * Mutliply or Divide the count of each node with val
	 */
	CoverTree<val_t>& operator*=(const double& val);
	CoverTree<val_t>& operator/=(const double& val);

	bool operator!=(const CoverTree<val_t>& other) const;
	bool operator==(const CoverTree<val_t>& other) const;

	CoverTreeNode<val_t> & operator[](int idx)
	{
		return _nodes[idx];
	}

	const CoverTreeNode<val_t> & operator[](int idx) const
	{
		return _nodes[idx];
	}

	typename CTNSeq<val_t>::iterator begin() {return _nodes.begin();}
	typename CTNSeq<val_t>::iterator end() {return _nodes.end();}
	typename CTNSeq<val_t>::const_iterator begin() const {return _nodes.begin();}
	typename CTNSeq<val_t>::const_iterator end() const {return _nodes.end();}

	/*
	 * Covert the Tree to a simple Text Representation
	 * doesn't show child - parent relationship only level of each node
	 */
	std::string to_string() const;

	/*
	 * Print the Tree to std::cout
	 */
	void print() const;


	friend std::ostream& operator<<(std::ostream& os, const CoverTree<val_t>& t)
	{
		os << t.to_string() << std::endl;
		return os;
	}
};

//The two types of values we are intersted in are double for first oder CoverTrees
//and CoverTree<double> for second order CoverTrees so we need to implement the
//get_count and update_count methode for booth;
double get_count(const double v);
double get_count(const CoverTree<double>& ct);

bool eq_count(const double v1,const double v2);
bool eq_count(const CoverTree<double> & ct1, const CoverTree<double> & ct2);

void update_count(double & v, const double n);
void update_count(CoverTree<double>& ct, const double n);

}

#endif // _OPENCOG_COVERTREE_H
