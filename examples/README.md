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
                  functions. ***VISIT THIS FIRST!!***

* flow          - The AtomSpace is not only a database for graphs,
                  but is also a system for streaming data through
                  processing networks described by graphs. This is
                  how sensory data is analyzed and transformed by
                  agents or organisms expressed in Atomese.

* query         - The graph query system shows how to find AtomSpace
                  contents that match a given subgraph or pattern.

* storage       - Moving Atoms and AtomSpaces between disk and memory
                  (i.e. RAM), or between network nodes ("Districuted
                  AtomSpace")

* python        - Python usage examples.

* type-system   - Creating and using new kinds of Atom Types.

* c++           - Example CMakefile and demos for C++ code.
* c++-guile     - Creating guile wrappers for C++ code.

* foreign       - Working with generic abstract syntax trees in the
                  AtomSpace. It shows how to store json, scheme and
                  python source code in the AtomSpace, and then search
                  for, and execute that code. (Experimental, incomplete)

Resources
---------
Some links to external sources that might help:
* [Luke’s Atomspace Bootstrap Guide](https://luketpeterson.github.io/atomspace-bootstrap-guide/)
  from Luke Peterson, 2020.
