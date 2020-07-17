
AtomSpace I/O and Persistence
-----------------------------

This directory contains code for reading, writing, storing, sending
AtomSpace contents to files, databases or network.  These are only
the "in project" systems for doing this; there are also some other
git repos that provide I/O backends.

Local subdirectories include:

* guile    -- The scheme module for the generic peristance API.
              It provides open, close, load and store primitives that
              work for any of the I/O back-ends, including those not
              in this repo (there are at least three others.)

* gearman  -- Experimental support for distributed operation, using
              GearMan.

* sexpr    -- Decode Atomese from s-expression UTF-8 strings.
              Includes a file-read utility.

* sql      -- Postgres, for now. Works well for most uses -- with caveats.
              Mostly, it's slow, running at approx 3K Atoms/sec.

The gearman and sql systems support networked, peer-to-peer communications
between AtomSpaces.

Semantics
---------
The correct semantics for the save and restore of values, including
truth values, an be subtle: during recrusive saves or loads of outgoing
sets, should values be clobbered? Left untouched? Merged?  The various
possibilities all have different performance implications, as well as
usability implications. These are discussed in the README.md file for
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
