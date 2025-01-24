AtomSpace Examples
------------------

This directory contains various examples and demos that illustrate
what the [AtomSpace](https://wiki.opencog.org/w/AtomSpace) and
[Atomese](https://wiki.opencog.org/w/Atomese) is.  You will want to
go through the contents here, in the order below. Keep in mind that
*every* [Atom](https://wiki.opencog.org/w/Atom) and
[Value](https://wiki.opencog.org/w/Value)
[type](https://wiki.opencog.org/w/Atom_types) is documented in the
wiki.

* atomspace     - Demos of all of the basic AtomSpace features and
                  functions. VISIT THIS FIRST!!

* pattern-matcher - Example code demonstrating the graph query system.
                  It shows how to search for data that fits a pattern,
                  how to search for patterns that fit the data, and
                  how to trigger actions when these are found.

* foreign       - Working with generic abstract syntax trees in the
                  AtomSpace. It shows how to store json, scheme and
                  python source code in the AtomSpace, and then search
                  for, and execute that code. (Experimental, incomplete)

* python        - Python usage examples.
* ocaml         - OCaml usage examples.
* haskell       - Haskell usage examples.

* c++           - Example CMakefile and demos for C++ code.
* c++-guile     - Creating guile wrappers for C++ code.

* type-system   - Creating and using new kinds of Atom Types.

Some of the examples require addtional modules that are not in the
core AtomSpace. These modules provide I/O to disk and network. They
can be obtained at
   https://gitub.com/opencog/atomspace-storage
and
   https://gitub.com/opencog/atomspace-rocks

The build and install steps are identical to those for the AtomSpace.
The `atomspace-storage` component provides a generic network and file
system API, while `atomspace-rocks` adapts it for the RocksDB database.

Some of the demos mention the Postgres and the CogServer (network)
storage nodes. Get these with
   https://gitub.com/opencog/atomspace-pgres
   https://gitub.com/opencog/atomspace-cog
   https://gitub.com/opencog/cogserver

Resources
---------
Some links to external sources that might help:
* [Luke’s Atomspace Bootstrap Guide](https://luketpeterson.github.io/atomspace-bootstrap-guide/)
  from Luke Peterson, 2020.
