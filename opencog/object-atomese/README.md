Objective Atomese
=================
An object-oriented infrastructure, in Atomese.  Version 0.0.0; there is
nothing here yet, except for a sketch of ideas.

Motivation
----------
The primary goal is to replicate some fraction of the code in the
[matrix](../matrix) directory, but to do so in pure
[Atomese](https://wiki.opencog.org/w/Atomese), instead of scheme. The
matrix code has proven to be exceptionally useful and powerful, but there
is now a need to be able to run it in (parallel) pipelines (configured
with [ProxyNode](https://wiki.opencog.org/w/ProxyNode)s.) This includes
running it over the network (via
[CogStorageNode](https://wiki.opencog.org/w/CogStorageNode)) or otherwise
interacting with [StorageNode](https://wiki.opencog.org/w/StorageNode)s
in interesting, non-trivial ways. For that, the matrix code must become
specifiable in Atomese. (Yes, we could just wrap it in a
[GroundedProcedureNode](https://wiki.opencog.org/w/GroundedProcedureNode)
but that would be ugly and counter-productive.)

Object Basics
=============
The actual code 


