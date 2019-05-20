Distributional Values
=====================

An implementation of Distributional Values that might eventually replace all current TruthValues.

Issues and Goals
----------------

One of the problems this is trying to solve is the combination of fuzzy and propabalistc logic. As both are used within OpenCog it is important to not only be able to clearly represent and distinguise them but also use them together in a combined for. That way no matter if the alogrithm expects fuzzy or propabalistc logic values they can work on a DistributionalValue. Which improves interoperability.
Another issue this improves upon is the accuracy and detail of TruthValues albeit at the cost of higher complexity.
Going beyond TruthValues this would also allow us to store and work with other distributions not only about the Truthness of a something about any kind of attribute.

CoverTree
---------

Our base data Structure is a CoverTree. CoverTrees are designed to speed up nearest neighbor search. And because CoverTrees require only a distanc metric to construct they work well in N-dimensions which is required for your usecase.

More detailed information can be found in the paper "Faster Cover Trees" by Mike Izbicki and Christian R. Shelton
https://izbicki.me/public/papers/icml2015-faster-cover-trees.pdf

CTHist
------

A Histogram based on a CoverTree. This is basically a CoverTree with some addtions and alterations. Primarly that it has a maximum number of elements. This necesitates some changes to the insert function so when we have already inserted the max number of elements any addtional data points will get merged with their nearest neighbor.
We also add a modified nearest neighbor search that considers direction. Which is necesary to find the Nodes surrounding a point, so we can average their values to get an aporixmation of the value at the given point. 
This is important for us because when we have mutliple distributions about the same underlying variable the bins will likely not line up. So if we can remap one Distribution by taking the bins of the other and getting the averaged value for those bins in the Distribution we can solve this.
With this we have a way to represent Joint Probability Distributions where each Variable coresponds to a dimensions in the Histogram.

DistributionalValue
-------------------

A OpenCog Value representing a Dirichlet Distribution. Each category of this Distribution corresponds to a bin in the underlying CTHist/Histogram. 
Which are simplified such that each bin only stores the center of a interval. While this is somewhat less accurate it makes up for it by simplifying and speeding up certain calculations in higher dimensions.

ConditionalDV
-------------

Similary to a DistributionalValue but containing a second order CTHist were each node containsa CTHist<double>.
Which allows a better TV trepresentation for InheritanceLinks and other similar links.

DVFormulas
----------

Various formulas for doing calculations with DistributionalValues and ConditionalDVs that don't fit directly in either class.
