
MST Benchmark
-------------
The MST parser is part of the backbone to the languagelearning project.
It might someday become a replacement to the backward chainer. Thus,
the performance is important.

The only algo currently in the sheaf directory is a modified Boruvka
algroithm, modified to perform only planar parses.  It runs in O(N^3)
time, based on the measurements done here.

State of the art algos are supposed to run at O(N). (???)

Boruvka's is supposed to run at about O(N^2) time,  So .. something is
wrong with the implementation, it would seem!?

One issue is that there is no a-priori way of knowing which edges exist,
and which do not, and therefore, N^2 possible edges need to be checked.
