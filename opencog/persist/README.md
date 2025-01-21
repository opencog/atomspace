
AtomSpace I/O and Persistence
-----------------------------

This directory contains code for reading, writing, storing, sending
AtomSpace contents to files, databases or network.  This repo provides
the core API, a file backend, and am SQL backend; other git repos
provide a RocksDB backend (`atomspace-rocks`) and a network backend
(`atomspace-cog`). The old Postgres backend has been moved to the
`atomspace-pgres` git repo.

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

* flow     -- Implement the FetchValueOfLink and the StoreCalueOfLink
              These allow specific Values to be obtained from storage,
              with Atomese. That is, it allows complex Atomese scripts
              to be written, that will work with Storage.

* json     -- Read and write Atomese JSON expressions. Suitable for
              web applications. Note, however, the `sexpr` interface
              is superior for performance and function.

* metta    -- Import and export fragments of MeTTa as Atomese. This
              provides only a fragment of MeTTa, currently consisting
              only of function declarations.

* prolog   -- Import and export fragments of prolog (datalog) as
              Atomese. This presumes only the simplest, most basic
              mapping: `:- siblings(jack, jill).` becomes
              `(Evaluation (Predicate "siblings") (List (Concept "jack") (Concept "jill")))`
              This is just enough to encode prolog facts and Horn
              clauses as Atomese.

* proxy    -- Implements a hierarchy of StorageNodes that act as agents.
              This includes a ReadThruProxy and a WriteThruProxy, that
              will pass storage I/O requests on to other StorageNodes.
              This is useful for configuring complex I/O pipelines
              in Atomese.

* sexpr    -- Read and write Atomese s-expressions as UTF-8 strings.
              Used both by the `FileStorageNode` and by `CogStorageNode`,
              which works in conjunction with the CogServer to provide
              network-distributed AtomSpaces.

* sexcom   -- Respond to a very special set of 17 s-expression commands.
              These are used to provide high-speed network I/O for the
              CogServer to provide network-distributed AtomSpaces.

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
