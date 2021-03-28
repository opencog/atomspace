
Correlation/Covariance Matrix Analysis Tools
============================================
[See also: key ideas presentation](docs/AtomSpace.pdf).

In this project, there's a generic theme of "pairs of things" that
are statistically related. These can be pairs of words, they can be
connector-sets, which are a pair of (word, disjunct), or they can
any kind of general pairs, correlating events and actions, causes
and effects, or pair-wise relationships of any sort. A recurring
problem is to obtain statistics for these pairs, and to manipulate
them in various ways.

A common theme in machine learning are "vectors of data": a vector
of numbers can be associated to some object, and one is interested in
classifying these vectors in some way; e.g. by using Principal Component
Analysis (PCA), Singular Value Decomposition (SVD), K-means clustering,
and so on. Vectors are buried in essentially all neural-net type
algorithms.  But - this is key: a collection of vectors can be viewed
as a matrix. Entries in the matrix are (row, column) pairs.

The code in this directory exposes portions of the AtomSpace as pairs,
or as a matrix, or as a collection of vectors, depending on how you want
to think about it.  It implements the low-level code for this access,
so that high-level algorithms can be implemented on top of it: so that
they can grab that data, do things with it, and write it back.  It
provides a "window" onto a portion of the AtomSpace, and everything
seen through that "window" looks like vectors, like one big matrix.

That is, the structure of interest are ordered pairs `(x,y)` of atoms
(that is, where `x` and `y` are atoms), along with any values attached
to such pairs. The primary value of interest is an observation count
`N(x,y)` of how often that particular pair was observed.  From this
observation count, one can compute various statistical measures: first,
the normalized frequency `p(x,y)` (that is, the probability, likelihood)
of how often the pair `(x,y)` occurred (the likelihood of observing the
pair).  These counts and frequencies can be viewed as a sparse correlation
matrix, and the goal here is to do all the typical things that one
might do with such a matrix.  That's what the code in this directory
does.

The prototypical example is that of word-pairs. These are stored in the
AtomSpace as
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
in relations between, say, `ConceptNodes` and `ContextLink`s. For
biology, the left side might be a GeneNode, and the right side a
ProteinNode.

The core idea is that the AtomSpace can hold sparse matrix data; in a
certain sense, the AtomSpace was designed from the get-go to do exactly
that. Once you realize that your data can be seen as a kind of matrix,
you can then apply a variety of generic matrix analysis tools to it.

Another way to arrive at this idea is to view your data as a graph,
(of vertexes connected with edges) and then realize that any graph can
be described in terms of it's adjacency matrix. This directory provides
tools to work with a graph from the point of view of its being an
adjacency matrix.

Thus, generically, a "matrix" in the AtomSpace is simply a collection of
Atoms, all of the same kind and general structure, with some
identifiable sub-part, called "the rows", or the "left atoms", and some
other identifiable sub-part, called "the columns", or the "right atoms".
As long as one can easily identify all of the Atoms that belong to the
collection, and one can identify two distinct sub-parts, one has
everything one needs to identify pairs `(left, right)` aka
`(row, column)` aka `(x,y)`.  To get a matrix, one needs one more thing:
a Value, some Value, any Value, holding a floating-point number for each
pair. This number is called `N(x,y)`. This number can represent any
numeric data at all.  In the prototypical usage, this number is an
observation count obtained by sensory data flowing in from the outside
world. It doesn't have to be - the matrix toolset provided here is
generic, intended to be useful for any AtomSpace data that meets the
requirement of having a numeric value attached to a collection of pairs.

The tools implemented here include:

 * Row and column subtotals (i.e. "marginals").
 * Computing and caching frequencies from counts.
 * Computing and caching mutual information of pairs.
 * Computing and caching marginal mutual information.
 * Computing cosine similarity between rows or columns.
 * Performing PCA (principal component analysis) in the matrix.
 * Performing cuts, to remove unwanted rows, columns and individual entries.

To use these tools, all you need to do is to specify a low-level
object that describes the matrix. If the matrix is in the form of an
`EvaluationLink`, as shown above, then it is sufficient to use the
`make-evaluation-pair-api` to describe the types of the left and right
(row and column) atoms, and the `PredicateNode` identifying the pair;
everything else is automated. If the matrix is not of the above form,
then it is quite easy to write a small, custom matrix-access object
that defines what the pairs are. See `object-api.scm` for details;
in short, one provides some very simple methods: the `'left-type` and
`'right-type` methods, that return the atom type of the rows and the
columns; a `'pair-type` method that returns the atom-type of the pair,
and a `'pair-count` method that returns the count, given the pair.


FAQ
---
**Q:** Why isn't this in C++?  Surely, numerical computations would be
   a lot faster in C++, right?

**A:** Yes, probably. But its a lot easier to write scheme code than
   it is to write C++ code, and so prototyping in scheme just made
   more sense. It was just-plain simpler, faster, easier (for me).
   You are invited to take the lessons learned, and re-implement
   in C++.

**Q:** What were the lessons learned?

**A:** The number #1 most important lesson is that the filter object
   is the most important object: it controls what data you want
   to keep, and what data you want to discard, and it needs to
   run "underneath", as a foundation to everything else.

**Q:** Really, C++ is sooo fast...

**A:** Yes, but since the data is stored in values associated with
   atoms in the AtomSpace, adding numbers together is NOT the
   bottleneck. Accessing Atom Values *is* the bottleneck. Finding
   a good performance optimization for the atom values framework
   is a lot harder.

**Q:** Why don't you just export all your data to SciPy or to Gnu R, or to
   Octave, or MatLab, for that matter, and just do your data analytics
   there?  That way, you don't need to re-implement all these basic
   statistical algorithms!

**A:** That sounds nice, but frankly, it's too much work for me. Maybe you
   can do this.  Seriously, its just a lot easier (for me) to create
   and use the code here, than to struggle mightily with those packages.

   Also: realize that the end-goal of OpenCog is not to export data
   once, analyze it once, and be done. Rather, the goal is to constantly
   and continuously monitor external, real-world events, pull them into
   the AtomSpace, crunch it incessantly, and update knowledge of the
   external world as a result. This rules out GUI tools for data
   processing (because there's no GUI in a server) and it rules out
   popsicle-stick architectures as being a bit hokey.

   Also: many of these structures have millions of rows and columns,
   and ten-million or a hundred-million non-zero entries in the matrix.
   This can blow through huge amounts of RAM. Can SciPy actually deal
   with datasets this big?  It gets worse, because typically, you want
   to work with different cuts and filters, where you discard much of
   the data, or average together different parts: can you really afford
   the RAM needed to export all of these different cut and filtered
   datasets?  Maybe you can, its just not trivial. (In my datasets,
   petabytes of RAM would be needed for non-sparse representations.)

**Q:** But if I did want to do it for Gnu R, how could I do it?

**A:** You would use Rcpp at http://dirk.eddelbuettel.com/code/rcpp.html
   Quote:
   *The Rcpp package provides C++ classes that greatly facilitate
   interfacing C or C++ code in R packages using the .Call() interface
   provided by R. Rcpp provides matching C++ classes for a large
   number of basic R data types. Hence, a package author can keep his
   data in normal R data structures without having to worry about
   translation or transferring to C++. At the same time, the data
   structures can be accessed as easily at the C++ level, and used in
   the normal manner. The mapping of data types works in both
   directions. It is as straightforward to pass data from R to C++,
   as it is it return data from C++ to R.*

**Q:** Any other design issues?

**A:** Yes. As currently structured, all of these classes assume that your
   data is readily available in RAM. They will not work correctly, if
   your dataset is too big to fit into RAM.  At the time of this
   writing, a single atom takes about 1.5KBytes or so, so a dataset
   consisting of 100M atoms will require about 150GBytes of RAM, plus
   a bit more for other processes (e.g. Postgres). Since most computers
   max out at about 256 GBytes RAM, this limits datasets to 100M atoms.
   Some language datasets can be considerably larger than this.
   The largest Amazon EC2 instances are 256 GBytes.


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
software systems (such as SciPy or Gnu Octave or tensorflow) could
be used to perform the analysis.  Now that you've got the general
idea... you can go do this!

Anyway, if you are willing to use scheme, here's what we've got,

Low-level API
-------------
Every user needs to implement a "low-level API" object that describes
the pairs, and provides methods to get them, and to get the associated
count (or other numeric value).

The methods that need to be implemented are described in
`object-api.scm`; a working example is in `eval-pair.scm`.
Additional working examples of the base classes can be found in
https://github.com/opencog/learn/tree/master/scm/batch-word-pair.scm
and in
https://github.com/opencog/learn/tree/master/scm/pseudo-csets.scm

Basic definitions
-----------------
Some notation:

Let `N(x,y)` be the observed count on the pair of atoms `(x,y)`.

The `add-support-api` class provides an API to report the partial
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

The `add-support-compute` class provides methods to compute the
partial sums `N(*,y)` and `N(x,*)`. It also provides methods that
compute how many non-zero entries there are in each row or column.
It also provides methods for the "length" of a column: that is,
`len(y) = sqrt(sum_x N^2 (x,y))` and more generally the l_p norm.
Because these computations can take a considerable amount of time,
the partial sums (the "marginals") are cached. The cached values can
be accessed with the `add-support-api` object.

The `make-compute-freq` class provides methods to compute and cache
the frequencies `p(x,y)`, `p(*,y)` and `p(x,*)`.  These are cached
exactly where the `add-pair-freq-api` class, above, can find them.

The `make-batch-mi` class provides methods to compute the fractional
mutual information of all pairs, namely the value
```
    MI(x,y) = +log_2 P(x,y) / P(x,*) P(*,y)
```

The `batch-all-pair-mi` class is a convenience class that wraps up all
three classes above, and unleashes them on a dataset: first computing
the row and column partial counts, then computing the frequencies, and
then computing and caching the mutual information.  For large datasets,
e.g. tens of millions of atoms, this can take hours to run.  Thus, for
this reason, the cached values are then saved to the currently-open
database, so that these results become available later.

Computing entropy
-----------------
The `add-pair-mi-compute` class provides methods to compute the entropy
and mutual information of rows and columns: for example, the column
entropy (or `left-entropy`) `h_left(y) = -sum_x P(x,y) log_2 P(x,y)`
It also returns the far more interesting 'fractional entropy', given
by `H_left(y) = h_left(y) / P(*,y)`.  Along similar lines, there is
also the mutual information `mi_left(y) = sum_x P(x,y) log_2 MI(x,y)`
where `MI(x,y) = +log_2 P(x,y) / P(x,*) P(*,y)` is the fractional pair
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


Direct sums
-----------
If one has a collection of things that can be vectors in two different
ways, then the direct sum can be used to create a single vector out of
the two.  It can be thought of as a concatenation of the two vectors,
appending one to the other to create a single vector.

For example, a word "foo" can be associated with a vector of disjuncts.
It can also be associated with a vector of connectors that it appears
in (a "shape"). These are two completely different kinds of vectors;
all they have in common is that both are associated with the word "foo".
As matrices, one consists of (word,disjunct) pairs, while the other
consists of (word,shape) pairs. The left-side of the matrix (the rows
of the matrix) all have the same type (i.e. words) while the right
side (the columns) are two different types (and are completely disjoint,
as sets). Concatenating these two matrices, placing one after the other,
results in a single matrix where the rows are result of appending the
rows of the second matrix to the first. If the dimensions of the two
matrices are N x M and N x K, then the dimensions of the combined matrix
would be N x (M + K).

The `direct-sum` class will combine these two matrices to provide an
object that behaves like a single matrix. The left and right basis
elements will be the set-union of the left and right basis elts of each
component matrix. By assumption, the types of either the columns or
the rows are distinct, and so one of these sets is disjoint.  Thus, the
union is unambiguous; the matrices are either placed side-by-side by
concatenating rows, or above-below, by concatenating columns. There is
no "overlap".  The total number of non-zero entries in the combined
matrix is the sum of the number of non-zero entries in each component.

(Caution: this is not the conventional definition of a direct sum, which
results in block-diagonal matrix. The conventional definition takes the
disjoint-union of the indexes (the basis), whereas here, we take the
set-union of the basis. The set-union makes more sense in the current
context, because the rows and columns have explicit labels, and it is
the labels that are important. The set union is obtained from the
disjoint union by means of a projection.)


Working with rows and columns
-----------------------------
The `add-tuple-math` class provides methods for applying arbitrary
functions to arbitrary sets of rows or columns. The simplest examples
include taking the sums and differences of columns, taking the
element-by-element min or max of a set of columns, counting the number
of entries that are simultaneously non-zero in sets of columns, etc.

The class works by defining a new matrix, whose rows or columns
are tuples (pairs, triples, etc) of the underlying matrix. For example,
one can ask for the matrix entry `(x, [y,z,w])`.  The value returned
will be `(x, f((x,y), (x,z), (x,w)))` where the user-defined function
`f` was used to create the `x` row.


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

The code is beta; only a bare minimum of function is provided. So far,
PCA is not terribly interesting for the kinds of graph problems we are
interested in solving.


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
 * The "star" objects need to be redesigned. They fetch wild-card counts
   and other marginal values straight out of the AtomSpace.  But those
   marginal values are correct only if no filtering is applied. If there
   are pre-filters, then the returned marginals are garbage. Yucko.  Can
   we fail-safe this for now?

 * Need to support columns/rows that can be one of several types
   (e.g. can be WordNodes, or be WordClassNodes) This is currently
   handled in an ad hoc manner.

 * Need a more consistent/coherent API for interacting with storage.
   Currently, the `fetch` methods do this, and there is a scattering
   of other store methods provided in an ad hoc, as-needed basis. All
   this should be made prettier.
