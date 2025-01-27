Conversion to columns
---------------------
GPU's are inherently SIMD machines, and want to get their data either
as long lists of uniform rows, or as long columns -- e.g. Apache Arrow
The intended use of the classes here is to facilitate the feeding of
sparse vector data into Apache Arrow.

To be more precise: any given pattern search over the AtomSpace will
resuls in a long vector of matching results. Attached to each item in
this vector will be some data, often floating point data. Thus, a goal
is to perform the search, convert the search results to s-expressions
(which are to be used as a UUID for an Atom), forming one column, and
then grab some numeric data out of each result, forming a second
floating-point vector column.
