Distributional Values
=====================

An implementation of Distributional Values that might eventually replace all current TruthValues.

Problem
-------

One of the problems this is trying to solve is the combination of fuzzy and propabalistc logic. As both are used within OpenCog it is important to be able to clearly represent and distinguise them.

Representation
--------------

To solve these issues we are using a Dirichlet Distribution where each category coresponds to the bin in a Histogram. As we need to do complex operations in potentially high dimensional spaces with these Histograms we only store the center of a bin instead of a complete Interval. These Mutlidemensional bins are used to store a Joint Probability Distribution over n Concepts in the Histogram.

For storing expclicity conditional Distribution Values we use a second order Histogram where each bin contains not a count but instead a first order Histogram.  These can be used for InheritanceLinks and the like.

To efficently work with these Histograms they utilize a Cover Tree for storage. 

CoverTree
---------
CoverTrees are a data structure designed to speed up nearest neighbor search.
And because CoverTrees require only a distanc metric to construct they work well in N-dimensions which is important for us.
More detailed information can be found in the paper "Faster Cover Trees" by Mike Izbicki and Christian R. Shelton
https://izbicki.me/public/papers/icml2015-faster-cover-trees.pdf

CTHist
------
A Histogram based on a CoverTree. This is basically a CoverTree with some addtions and alterations. Primarly that it has a maximum number of elements. This necesitates some changes to the insert function so when we have already inserted the max number of elements any addtional data points will get merged with their nearest neighbor.
We also add a modified nearest neighbor search that considers direction. Which is necesary to find the Nodes surrounding a point, so we can average their values to get an aporixmation of the value at the given point. 
This is important for us because when we have mutliple distributions about the same underlying variable the bins will likely not line up. So if we can remap one Distribution by taking the bins of the other and getting the averaged value for those bins in the Distribution we can solve this.
