
AtomSpace I/O and Persistence
-----------------------------

This directory contains code for reading, writing, storing, sending
AtomSpace contents to files, databases or network.  This repo provides
the core API, a file backend, and am SQL backend; other git repos
provide a RocksDB backend (`atomspace-rocks`) and a network backend
(`atomspace-cog`).

Local subdirectories include:

* api      -- The generic StorageNode API.
              It provides open, close, load and store primitives that
              work for any of the I/O back-ends, including those not
              in this repo (there are at several others, including one
              for RocksDB and one that allows AtomSpaces to trade
              Atoms over the network.)

* csv      -- Load Values from CSV/TSV files. These are "delimiter
              separated values" -- ordinary tables. Each column in the
              table is loaded into an appropriate Value (`FloatValue`,
              `BoolValue` or `StringValue`). The values are placed
              under keys (named after the column) on the provided Atom.
              This is intended for the ASMOSES subsystem, which
              naturally operates on tables or streams of data.

* file     -- Read and write files containing Atomese s-expressions.
              Provides both a `FileStorageNode`, and also some utilities
              to read files, and dump Atomspace contents to files or
              guile ports (without having to use `StorageNode`s.)

* gearman  -- Experimental support for distributed operation, using
              GearMan. Unused, unsupported, deprecated, more or less.
              If you are interested in this, contact the mailing list.

* json     -- Read and write Atomese JSON expressions. Suitable for
              web applications. Note, however, the `sexpr` interface
              is superior for performance and function.

* sexpr    -- Read and write Atomese s-expressions as UTF-8 strings.
              Used both by the `FileStorageNode` and by `CogStorageNode`,
              which works in conjunction with the CogServer to provide
              network-distributed AtomSpaces.

* sql      -- Postgres, for now. Works OK for most uses -- with caveats.
              Mostly, it's slow, running 3x slower than the RocksDB
              backend. It also fails to support some newer AtomSpace
              features. It really needs to rewritten from scratch.
              The best rewrite would start with the RocksDB backend,
              and port it to Postgres.

* sql      -- The only "good thing" about the Postgres backend is that
              multiple networked machines can attach to a single Postgres
              server, and thus they can share Atoms via Postgres. So,
              simple distributed operation.

* tlb      -- Implements a table that issues a unique integer ID for an
              Atom. Useful, if you think your code needs to substitute
              integer ID's for Atoms. Note that doing this consumes both
              RAM, to store the table, and CPU, to perform lookup. So it
              is probably not a good idea, in general. But if you really
              really need this, well, here it is.


Semantics
---------
The correct semantics for the save and restore of values, including
truth values, can be subtle: during recursive saves or loads of outgoing
sets, should values be clobbered? Left untouched? Merged?  The various
possibilities all have different performance implications, as well as
usability implications. These are discussed in the `README.md` file for
the SQL implementation, and semantics are explicitly tested in the SQL
unit tests.

Right now, you are provided with mechanism, not policy. You have the
tools, and you can implement whatever policy you want.


TODO
----
Create a `ProxyBackingStore` and/or `ProxyStorageNode` that implements
various different kinds of storage policies. This includes:
* Multiplexing reads & writes to other backends.
* Providing an expiring cache (old atomspace contents are cleared out
  and thus are re-fetched as needed, else the cache is used.)
* Providing non-trivial data access authorization, including read-only
  access.

Future directions
-----------------
To understand the future directions for the (distributed) atomspace
and its relation to storage, read the "Big Graph Anti-Pattern" blog
post: https://blog.blazegraph.com/?p=628

OpenCog most closely resembles the fourth bullet in that post:
"Graph query (aka graph pattern matching)"  Based on this, the
most promising backend would seem to be "blazegraph":
http://sourceforge.net/projects/bigdata/ (GPLv2)
http://www.blazegraph.com/
