
AtomSpace I/O and Persistence
-----------------------------

This directory contains code for reading, writing, storing, sending
AtomSpace contents to files, databases or network.  This repo provides
the core API, a file backend, and am SQL backend; other git repos
provide a RocksDB backend (`atomspace-rocks`) and a network backend
(`atomspace-cog`).

Local subdirectories include:

* guile    -- The scheme module for the generic peristance API.
              It provides open, close, load and store primitives that
              work for any of the I/O back-ends, including those not
              in this repo (there are at several others, including one
              for RocksDB and one that allows AtomSpaces to trade
              Atoms over the network.)

* gearman  -- Experimental support for distributed operation, using
              GearMan. Unused, unsupported, deprecated, more or less.
              If you are interested in this, contact the mailing list.

* sexpr    -- Read and write Atomese s-expression as UTF-8 strings.
              Includes utilities to read files, and dump Atomspace
              contents to files or guile ports.

* sql      -- Postgres, for now. Works well for most uses -- with caveats.
              Mostly, it's slow, running at approx 3K Atoms/sec. Note
              that multiple AtomSpaces can attach to a single Postgres
              server, and thus they can share Atoms over the network in
              this fashion.  Note that it is almost certanily faster (?)
              to use `atomspace-cog` for sharing Atoms over the network.


Semantics
---------
The correct semantics for the save and restore of values, including
truth values, can be subtle: during recrusive saves or loads of outgoing
sets, should values be clobbered? Left untouched? Merged?  The various
possibilities all have different performance implications, as well as
usability implications. These are discussed in the `README.md` file for
the SQL implementation, and semantics are explicitly tested in the SQL
unit tests.


Future directions:
------------------
To understand the future directions for the (distributed) atomspace
and its relation to storage, read the "Big Graph Anti-Pattern" blog
post: https://blog.blazegraph.com/?p=628

OpenCog most closely resembles the fourth bullet in that post:
"Graph query (aka graph pattern matching)"  Based on this, the
most promising backend would seem to be "blazegraph":
http://sourceforge.net/projects/bigdata/ (GPLv2)
http://www.blazegraph.com/
