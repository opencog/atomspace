
Correlation/Covariance Matrix Analysis Tools
============================================

In this project, there's a generic theme of "pairs of things" that
are statistically related. These can be pairs of words, they can be
connector-sets, which are a pair of (word, disjunct), or they can
be other things. A recurring question is how these things are related.

Thus, we are generally interested in pairs `(x,y)` of atoms (that is,
where `x` and `y` are atoms), and we have some sort of count `N(x,y)`
of how often that particular pair was observed.  We typically are then
interested in various statistical measures: usually starting with the
normalized frequency `p(x,y)` (that is, the probability, likelihood)
of how often the pair `(x,y)` occured (likelihood of observing the pair).
These counts and frequencies can be viewed as a sparse correlation
matrix, and the goal here is to do all the typical things that one
might do with such a matrix.  That's what the code in this directory
does.

The prototypical example is that of word-pairs. These are stored in the
atomspace as
```
    EvaluationLink   (count=6789)
        PredicateNode "word-pair"
        ListLink
            WordNode "foo"
            WordNode "bar"
```
which indicates that the word-pair (foo,bar) was observed 6789 times.
In the general, generic case, we might want to observe not just these
`PredicateNode "word-pair"` relations, but maybe some other kinds of
predicates. We are interested, perhaps, not just in `WordNode`'s but
in relations between, say, `ConceptNodes` and `ContextLink`s.

The core idea is that the atomspace can hold sparse matrix data; in a
certain sense, the atomspace was designed from the get-go to do exactly
that. Once you realize that your data can be seen as a kind of matrix,
you can then apply a variety of generic matrix analysis tools to it.
The tools implemented here include:

 * row and column subtotals (i.e. "marginals")
 * computing and caching frequencies from counts.
 * computing and caching mutual information between rows and columns
 * computing cosine similarity between rows or columns.
 * performing PCA (principal component analysis) in the matrix.
 * performing cuts, to remove unwanted rows, coumns and individual entries.

To use these tools, all you need to do is to specify a low-level
object that describes the matrix. It needs to provide some very simple
but important mthods: the `'left-type` and `'right-type` methods,
that return the atom type of the rows and the columns; a `'pair-type`
method that returns the atom-type of the pair, and a `'pair-count`
method that returns the count, given the pair.


FAQ
---
Q: Why isn't this in C++?  Surely, numericaal computations would be
   a lot faster in C++, right?

A: Yes, probably. But its a lot easier to write scheme code than
   it is to write C++ code, and so prototyping in scheme just made
   more sense. It was just-plain simpler, faster, esier. You are
   invited to take the lessons learned, and re-implement in C++.
   The number #1 most important lesson is that the filter object
   is the most important object: it controls what data you want
   to keep, and what data you want to discard, and it needs to
   run "underneath", as a foundation to everything else.

Q: Really, C++ is sooo fast...

A: Yes, but since the data is stored in values assocaited with
   atoms in the atomspace, adding numbers together is NOT the
   bottleneck. Accessing Atom Values *is* the bottleneck. Soooo...

Q: Why don't you just export all your data to SciPy or to Gnu R, or to
   Octave, or MatLab, for that matter, and just do your data analytics
   there?  That way, you don't need to re-implement all these basic
   statistical algorithms.

A: That sounds nice, but frankly, it's too much work for me. Maybe you
   can do this.  Seriously, its just a lot easier (for me) to create
   and use the code here, than to struggle mightlily with those packages.

   Also: realize that the end-goal of opencog is not to export data
   once, analyze it once, and be done. Rather, the goal is to constantly
   and continuously monitor external, real-world events, pull them into
   the atomspace, crunch it incessently, and update knowledge of the
   external world as a result. This rules out GUI tools for data
   processing (because there's no GUI in a server) and it rules out
   popsicle-stick architectures as being a bit hokey.

   Also: many of these structures have millions of rows and columns,
   and ten-million or a hundred-million non-zero entries in the matrix.
   This can blow through huge amounts of RAM. Can SciPy actually deal
   with datasets this big?  It gets worse, because typically, you want
   to work with different cuts and filters, where you discard much of
   the data, or average together different parts: can you relly afford
   the RAM needed to export all of these different cut and filtered
   datasets?  Maybe you can, its just not trivial.

Q: But if I did want to do it for Gnu R, how could I do it?

A: You would use Rcpp at http://dirk.eddelbuettel.com/code/rcpp.html
   Quote:
   The Rcpp package provides C++ classes that greatly facilitate
   interfacing C or C++ code in R packages using the .Call() interface
   provided by R. Rcpp provides matching C++ classes for a large
   number of basic R data types. Hence, a package author can keep his
   data in normal R data structures without having to worry about
   translation or transfering to C++. At the same time, the data
   structures can be accessed as easily at the C++ level, and used in
   the normal manner. The mapping of data types works in both
   directions. It is as straightforward to pass data from R to C++,
   as it is it return data from C++ to R. The following two sections
   list supported data types.


Generic Programming
-------------------
In order to perform some sort of generic analysis of the correlation
of atoms, we need to somehow specify which pairs of atoms are
the ones to be analyzed. This is done with a minimalist object-oriented
API, where you, the user, get to provide a "low-level object" which
indicates the types of the left and the right atoms, the type of the
pair, and where the counts `N(x,y)` are located.  This library then
implements a collection of various objects that sit "on top" of this,
and provide various reasonable default behaviors for computing the
various interesting things. If you don't like some particular default,
you can always overload that particular method to do something
customized. This OO programming style is called "parametric
polymorphism".

https://en.wikipedia.org/wiki/Parametric_polymorphism

https://en.wikipedia.org/wiki/Generic_programming

This code is written in scheme.  I know some of you want to use python
instead, while others would prefer C++.  More generally, it would
probably be useful to set things up so that external, third-party
software systems (such as scipy or Gnu Octave or tensorflow) could
be used to perform the analysis.  Now that you've got the general
idea... you can go do this!

Anyway, if you are willing to use scheme, here's what we've got,

Low-level API
-------------
Every user needs to implement a "low-level API" object that describes
the pairs, and provides methods to get them, and to get the associated
count (or other numeric value).

The methods that need to be implemented are described in
`object-api.scm`. Working examples of the base classes can be found in
http://github.com/opencog/opencog/tree/master/opencog/nlp/learn/scm/batch-word-pair.scm
and in
http://github.com/opencog/opencog/tree/master/opencog/nlp/learn/scm/pseudo-csets.scm

Basic definitions
-----------------
... and some notation to go with it:

Let `N(x,y)` be the observed count on the pair of atoms `(x,y)`.

The `add-pair-count-api` class provides an API to report the parital
sums `N(x,*) = sum_y N(x,y)` and likewise `N(*,y)`.  If you think of
`N(x,y)` as a matrix, these are the totals for the entries in each
row or column of the matrix. Likewise, `N(*,*) = sum_x sum_y N(x,y)`.
In statistics, these are called "marginals", because you write the
subtotals in the "margins" of your table.

The `add-pair-freq-api` class provides an API to report the frequencies
of pairs, and the partial sums over rows and columns. The frequency
is defined, by default, to be `p(x,y) = N(x,y)/N(*,*)`.  The row and
column sums are `p(x,*) = sum_y p(x,y)`.  By default, these total to
one, as all good probabilities should: `1 = sum_x sum_y p(x,y)`.

The `add-report-api` class provides an API to report summary information
about the matrix, including the dimensions of the matrix (the number of
rows and columns), the total number of non-zero entries (which is the
same as the total number of unique pairs), the left, right and total
entropies and mutual information.

Computing basic statistics
--------------------------
These `add-pair-*-api` classes simply provide methods to fetch these
values, which are presumed to have been precomputed in some way. Several
classes are provided to compute these.

The `add-pair-stars` class provides methods to fetch the set
`{x | the atom x occurs in some pair (x,y)}`.  This is called the
`left-support-set`. Likewise for the right-support.
Another method returns the wild-card pairs: that is, the set
`(x,*) = {(x,y) | x is fixed and y is any atom in the pair}`
This is called the `right-star` because the right-side of the pair is
a wild-card.  These methods are useful for iterating over all rows and
columns of the correlation matrix.

The `add-loop-api` class provides methods to loop over all non-zero
pairs in the matrix, and invoke a function on them. There are two
methods:  `for-each-pair`, which simply calls the function for each
pair, and a `map-pair` method which returns a list of the results of
calling the function on each pair.

The `make-compute-count` class provides methods to compute the partial
sums `N(*,y)` and `N(x,*)` and cache the resulting values on atoms where
they can be quickly retreived. The location of the cached values are
exactly where they can be found by the `add-pair-count-api`, above.

The `make-compute-freq` class provides methods to compute and cache
the frequencies `p(x,y)`, `p(*,y)` and `p(x,*)`.  These are cached
exactly where the `add-pair-freq-api` class, above, can find them.

The `make-batch-mi` class provides methods to compute the fractional
mutual information of all pairs, namely the value
```
    MI(x,y) = -log_2 P(x,y) / P(x,*) P(*,y)
```

The `batch-all-pair-mi` class is a convenience class that wraps up all
three classes above, and unleashes them on a dataset: first computing
the row and column partial counts, then computing the frequencies, and
then computing and caching the mutual information.  For large datasets,
e.g. tens of millions of atoms, this can take hours to run.  Thus, for
this reaso, the cached values are then saved to the currrently-open
database, so that these results become available later.

Computing support and entropy
-----------------------------
The `add-support-compute` class provides methods to compute the
partial sums `N(*,y)` and `N(x,*)`. It also provides methods that
compute how many non-zero entries there are in each row or column.
It provides methods for the "length" of a column: that is,
`len(y) = sqrt(sum_x N^2 (x,y))` and more generally the l_p norm.

The `add-pair-mi-compute` class provides methods to compute the entropy
and mutual information of rows and columns: for example, the column
entropy (or `left-entropy`) `h_left(y) = -sum_x P(x,y) log_2 P(x,y)`
It also returns the far more interesting 'fractional entropy', given
by `H_left(y) = h_left(y) / P(*,y)`.  Along similar lines, there is
also the mutual information `mi_left(y) = sum_x P(x,y) log_2 MI(x,y)`
where `MI(x,y) = -log_2 P(x,y) / P(x,*) P(*,y)` is the fractional pair
MI.

The `add-total-entropy-compute` class provides methods to compute the
total entropy: `H_tot = sum_x sum_y p(x,y) log_2 p(x,y)` as well as
the left and right entropies: `H_left = sum_y p(*,y) log_2 p(*,y)`
and v.v.

Computing similarity measures
-----------------------------
The `add-pair-cosine-compute` class provides methods for taking the
vector product of two different rows or columns, viz. the product
`left-prod(y,z) = sum_x N(x,y) N(x,z)` and likewise for the right.
The cosine similarity is
```
   left-cosine(y,z) = left-prod(y,z) / (left-length(y) * left-length(z))
```
where
```
   left-length(y) = sqrt sum_x N(x,y) N(x,y)
                  = sqrt left-prod(y,y)
````
The class also provides the Jaccard similarity
```
   left-jacc-sim(y,z) = sum_x min (N(x,y), N(x,z)) /
            sum_x max (N(x,y), N(x,z))
```

The `add-tuple-math` class provides methods for applying aribitrary
functions to arebitrary sets of rows or columns. The simplest examples
include taking the sums and differences of columns, taking the
element-by-element min or max of a set of columns, counting the number
of entries that are simultaneously non-zero in sets of columns, etc.


Principal Component Analysis
----------------------------
A power iteration object is provided by the `make-power-iter-pca`
object.  Once a matrix `X` has been specified, this will iterate on
either `X^T X` or on `XX^T` for `K` iterations.  The result of this
iteration is the principal component of `X` (equivalently, the
Frobenius-Peron eigenvector of `X^T X` or `XX^T`).

The code designed to work fast for sparse matrices, and can obtain
eigenvectors in under 20 seconds or so for 15K by 15K matrices with
100K non-zero entries.

The code is beta, in active development: only a bare minimum of function
is provided.


Data Cuts
---------
Data will typically have lots of "noise" in it, which should be masked
before data analysis starts. The tools in `filter.scm` can do this.  A
generic filter will mask out arbitrary rows, columns and individual
entries, depending on the supplied predicates.  Built on top of this is
a filter that masks out rows, columns and entries that have counts
below a threshold.  Another filter can mask out explicitly-named rows
and columns.


Tensors, in general
-------------------
Suppose you have more than just pairs. Suppose you have triples that
you want to work with. Then what?  Answer: use the network analysis
tools in the (opencog network) module.


TODO
----
To-do list items.
 * The "api" objects need to be redesigned. They fetch stuff out of
   the atomspace, which can only work if no filtering is applied. But
   if there are pre-filters, then the returned values are necessarily
   garbage. Yucko.  Can we fail-safe this for now?
