
MST Benchmark
-------------
The MST parser is part of the backbone to the languagelearning project.
It might someday become a replacement to the backward chainer. Thus,
the performance is important.

The only algo currently in the sheaf directory is a modified Boruvka
algroithm, modified to perform only planar parses.  It runs in O(N^3)
time, based on the measurements done here. (Actually O(N^2.9) for the
synthetic datasets used for the measurements).

State of the art algos are supposed to run at O(N).

Boruvka's is supposed to run at O(N log N) time,  So .. something is
wrong with the implementation, it would seem!?
