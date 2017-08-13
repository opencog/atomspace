
MST Benchmark
-------------
The MST parser is part of the backbone to the language learning project.
It might someday become a replacement to the backward chainer. Thus,
the performance is important.

The only algo currently in the sheaf directory is a modified Boruvka
algorithm, modified to perform projective (planar, no-edges-cross)
parses.  It runs in O(N^3) time, based on the measurements done here.

* Yuret published an O(N^3) algo.

* The "state of the art" projective MST algo is the "Eisner algo", and
  its O(N^3). So we currently match state-of-the-art.

In fact, we can beat state of the art, if we use a scoring function
that eliminates edge lengths of greater than 6. That algo runs at
O(N^3) up to about N=10, and thereafter runs at O(N^2.3) See
`boruvka-701K.png` and `boruvka-701K-limit.dat` for the raw data.

This is probably because the algo has some O(M) part to it, where
M is the number of edges, and the fast length-check in the scoring
function kicks it a notch.

==State of the Art

State of the art non-projective algos are supposed to run in O(M) where
M is the number of edges. Since we have to effectively explore N^2
edges, that still means that even a non-projective parse runs at O(N^2).

... Unless we length-limit the scores, as above.

Boruvka's is supposed to run at about O(N^2) time, however, it also
is a non-projective algo.  From what I can tell, the best algos are:

* Chu-Liu-Edmonds - non-projective MST, runs in O(N^2) time
* Eisner algorithm -projective MST, runs in O(N^3) time.

So the algo here runs at the same speed as the "state of the art".
Better, even, with the right scoring function! Woo hoo!
Actual performance can probably be improved by re-implementing in C++.

On the other hand, the O(N^2.3) behavior suggests that the algorithm is
bottlenecked in fetching scores from the atomspace.  If this really is
true, then re-writing in C++ might not help very much...

== References
* Eisner, Jason, 1996. “Three New Probabilistic Models for Dependency
  Parsing.” In Proceedings of the 16th Conference on Computational
  Linguistics (CoLING 96) . Saarbruecken: ACL, 340–345.
