OpenCog AtomSpace
=================

[![Build Status](http://61.92.69.39:8080/buildStatus/icon?job=ci-atomspace-master)](http://61.92.69.39:8080/job/ci-atomspace-master)

The OpenCog AtomSpace is a knowledge representation (KR) database and
the associated query/reasoning engine to fetch and manipulate that data,
and perform reasoning on it. Data is represented in the form of graphs,
and more generally, as hypergraphs; thus the AtomSpace is a kind of
graph database, the query engine is a general graph re-writing system,
and the rule-engine is a generalized rule-driven inferencing system.
The vertices and edges of a graph, known as "Atoms", are used to
represent not only "data", but also "procedures"; thus, many graphs
are executable programs as well as data structures.

There are pre-defined Atoms for many basic knowledge-representation and
computer-science concepts. These include Atoms for relations, such as
similarity, inheritance and subsets; for logic, such as Boolean and, or,
for-all, there-exists; for Bayesian and other probabilistic relations;
for intuitionist logic, such as absence and choice; for parallel
(threaded) synchronous and asynchronous execution; for expressions with
variables and for lambda expressions and for beta-reduction and mapping;
for uniqueness constraints, state and a messaging "blackboard"; for
searching and satisfiability and graph re-writing; for the specification
of types and type signatures, including type polymorphism and type
construction (dependent types and type variables TBD).

Because of these many and varied Atom types, constructing graphs to
represent knowledge looks kind-of-like "programming"; the programming
language is informally referred to as "Atomese".  It vaguely resembles
a strange mashup of SQL (due to queriability), prolog/datalog (due to
the logic and reasoning components), lisp/scheme (due to lambda
expressions), haskell/caml (due to the type system) and rule engines
(due to the graph rewriting and forward/backward chaining inference
systems).  This "programming language" is NOT designed for use by
human programmers (it is too verbose and awkward for that); it is
designed for automation and machine learning.  That is, like any
knowledge representation system, the data and procedures encoded
in "Atomese" are meant to be accessed by other automated subsystems
manipulating and querying and inferencing over the data/programs.
Also, viewed as a programming language, it can be very slow and
inefficient and not scalable; it was not designed with efficiency
and programming tasks in mind, nor with scalability; but rather, it
was designed to allow the generalized manipulation of networks of
probabilistic data by means of rules and inferences and reasoning
systems.  It extends the idea of probabilistic logic networks to a
generalized system for automatically manipulating and managing data.

The use of the AtomSpace, and the operation and utility of Atomese,
remains a topic of ongoing research and change, as various dependent
subsystems are brought online. These include machine learning,
natural language processing, motion control and animation, planning
and constraint solving, pattern mining and data mining, question
answering and common-sense systems, and emotional and behavioral
psychological systems.  Each of these impose sharply conflicting
requirements on the system architecture; the AtomSpace and "Atomese"
is the current best-effort KR system for satisfying all these various
needs in an integrated way.  It is likely to change, as the various
current short-comings, design flaws, performance and scalability
issues are corrected.

The main project site is at http://opencog.org

The [examples](https://github.com/opencog/atomspace/blob/master/examples)
directory contains demonstrations of the various components of the
AtomSpace, including the python and scheme bindings, the pattern
matcher, the rule engine, and many of the various different atom types
and their use for solving various different tasks.


Prerequisites
-------------
To build the OpenCog AtomSpace, the packages listed below are required.
With a few exceptions, most Linux distributions will provide these
packages. Users of Ubuntu 14.04 "Trusty Tahr" may use the dependency
installer at `/scripts/octool`.  Users of any version of Linux may
use the Dockerfile to quickly build a container in which OpenCog will
be built and run.

###### boost
* C++ utilities package.
* http://www.boost.org/ | `apt-get install libboost-dev`

###### cmake
* Build management tool; v2.8 or higher recommended.
* http://www.cmake.org/ | `apt-get install cmake`

###### cogutil
* Common OpenCog C++ utilities.
* http://github.com/opencog/cogutils
* It uses exactly the same build procedure as this package. Be sure
  to `sudo make install` at the end.

###### guile
* Embedded scheme REPL (version 2.0.9 or newer is required).
* http://www.gnu.org/software/guile/guile.html | `apt-get install guile-2.0-dev`

Optional Prerequisites
----------------------
The following packages are optional. If they are not installed, some
optional parts of the AtomSpace will not be built.  The CMake command,
during the build, will be more precise as to which parts will not be built.

###### cxxtest
* Test framework
* Optional but recommended; required for running unit tests.
* http://cxxtest.sourceforge.net/ | https://launchpad.net/~opencog-dev/+archive/ppa

###### Cython
* C bindings for Python.
* Strongly recommended, as many examples and important subsystems
  assume python bindings.
* http://cython.org | `apt-get install cython`

###### Haskell
* Haskell bindings (experimental).
* Optional; almost no existing code makes use of haskell.
* https://www.haskell.org/

###### Postgres
* Distributed, multi-client networked storage.
* Needed for "remembering" things between shutdowns.
* http://postgres.org | `apt-get install postgresql postgresql-client`

###### unixODBC
* Generic SQL Database client access libraries.
* Required for the distributed-processing atomspace.
* http://www.unixodbc.org/ | `apt-get install unixodbc-dev`

###### ZeroMQ (version 3.2.4 or higher)
* Asynchronous messaging library.
* Optional, almost completely unused, mostly due to poor performance.
* http://zeromq.org/intro:get-the-software | `apt-get install libzmq3-dev`

###### Google Protocol Buffers
* Google's data interchange format (used by ZeroMQ).
* Optional, needed only for ZMQ, above.
* https://developers.google.com/protocol-buffers | `apt-get install libprotobuf-dev`


Building AtomSpace
------------------
Perform the following steps at the shell prompt:
```
    cd to project root dir
    mkdir build
    cd build
    cmake ..
    make
```
Libraries will be built into subdirectories within build, mirroring
the structure of the source directory root.


Unit tests
----------
To build and run the unit tests, from the `./build` directory enter
(after building opencog as above):
```
    make test
```

Install
-------
After building, you MUST install the atomspace.
```
    sudo make install
```

Using the AtomSpace
-------------------
The AtomSpace can be used in one of three ways, or a mixture of all three:
By using the GNU Guile scheme interface, by using Python, or by running
the OpenCog cogserver.

Guile provides the easiest interface for creating atoms, loading them
into the AtomSpace, and performing various processing operations on
them.  For examples, see the `/examples/guile` and the
`/examples/pattern-matcher` directories.

Python is more familiar than scheme (guile) to most programmers, and
it offers another way of interfacing to the atomspace. See the
`/examples/python` directory for how to use python with the AtomSpace.

The OpenCog cogserver provides a network server interface to OpenCog.
It is required for running embodiment, some of the reasoning agents,
and some of the natural-language processing agents.  The cogserver is
only available in the main OpenCog project; it is not a part of the
AtomSpace.

CMake notes
-----------
Some useful CMake's web sites/pages:

 - http://www.cmake.org (main page)
 - http://www.cmake.org/Wiki/CMake_Useful_Variables
 - http://www.cmake.org/Wiki/CMake_Useful_Variables/Get_Variables_From_CMake_Dashboards
 - http://www.cmake.org/Wiki/CMakeMacroAddCxxTest
 - http://www.cmake.org/Wiki/CMake_HowToFindInstalledSoftware


The main CMakeLists.txt currently sets -DNDEBUG. This disables Boost
matrix/vector debugging code and safety checks, with the benefit of
making it much faster. Boost sparse matrixes and (dense) vectors are
currently used by ECAN's ImportanceDiffusionAgent. If you use Boost
ublas in other code, it may be a good idea to at least temporarily
unset NDEBUG. Also if the Boost assert.h is used it will be necessary
to unset NDEBUG. Boost ublas is intended to respond to a specific
BOOST_UBLAS_NDEBUG, however this is not available as of the current
Ubuntu standard version (1.34).
