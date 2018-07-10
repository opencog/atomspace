OpenCog AtomSpace
=================

master:
[![Build Status](http://61.92.69.39:8080/buildStatus/icon?job=ci-atomspace-master)](http://61.92.69.39:8080/job/ci-atomspace-master)
stable:
[![Build Status](http://61.92.69.39:8080/buildStatus/icon?job=ci-atomspace-stable)](http://61.92.69.39:8080/job/ci-atomspace-stable)

The OpenCog AtomSpace is a knowledge representation (KR) database and
the associated query/reasoning engine to fetch and manipulate that data,
and perform reasoning on it. Data is represented in the form of graphs,
and more generally, as hypergraphs; thus the AtomSpace is a kind of
graph database, the query engine is a general graph re-writing system,
and the rule-engine is a generalized rule-driven inferencing system.
The vertices and edges of a graph, known as "Atoms", are used to
represent not only "data", but also "procedures"; thus, many graphs
are executable programs as well as data structures.

The AtomSpace is a platform for building Artificial General Intelligence
(AGI) systems. It provides the central knowledge representation component
for OpenCog. As such, it is a fairly mature component, on which a lot of
other systems are built, and which depend on it for stable, correct
operation in a day-to-day production environment.

However, it turns out that knowledge representation is hard, and so the
AtomSpace is also a platform for active scientific research on knowledge
representation, knowledge discovery and knowledge manipulation.


Using Atomese and the AtomSpace
===============================
The AtomSpace is not intended for end-users. Rather, it is a knowledge-base
platform. It is probably easiest to think of it as kind-of-like an operating
system kernel: you don't need to know how it works to use it.  You probably
don't need to tinker with it. It just works, and its there when you need it.

End-users and application developers will want to use one of the existing
"app" subsystems, or write their own.  Most of the existing AtomSpace "apps"
are focused on various aspects of "Artificial General Intelligence". This
includes (unsupervised) natural-language learning, machine-learning,
reasoning and induction, chatbots, robot control, perceptual subsystems
(vision processing, sound input), genomic and proteomic data analysis,
deep-learning neural-net interfaces. These can be found in other github
repos, including:

* [Natural language, chat, robot control](https://github.com/opencog/opencog)
  (the opencog repo)
* [ROS bridge to robots, vision subsystem, chat](https://github.com/opencog/ghost_bridge)
  (ghost-bridge repo)
* [Unsupervised natural language learning](https://github.com/opencog/language-learning)
  (language-learning repo)
* [Genomic, proteomic data analysis](https://github.com/opencog/agi-bio)
  (agi-bio repo)
* [Opencog on a Raspberry Pi](https://github.com/opencog/tinycog)
  (tinycog repo)
* [Port of the MOSES machine learning to Atomese](https://github.com/opencog/as-moses)
  (as-moses repo)


A  Theoretical Overview
=======================
The AtomSpace is a mashup of a large variety of concepts from
mathematical logic, theorem proving, graph theory, database theory,
type theory and knowledge representation. Its hard to provide a
coherent overview without throwing around a lot of "big words" and
"big concepts".  We're trying to get a lot of things done, here,
and there's no particularly simple or effective way of doing it
without a lot of foundational theory.

### Atom Types
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
construction.

### Atomese
Because of these many and varied Atom types, constructing graphs to
represent knowledge looks kind-of-like "programming"; the programming
language is informally referred to as "Atomese".  It vaguely resembles
a strange mash-up of SQL (due to queriability), prolog/datalog (due to
the logic and reasoning components), lisp/scheme (due to lambda
expressions), Haskell/CaML (due to the type system) and rule engines
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
remains a topic of ongoing research and design experimentation, as
various AI and knowledge-processing subsystems are developed. These
include machine learning, natural language processing, motion control
and animation, deep-learning networks and vision processing,
constraint solving and planning, pattern mining and data mining,
question answering and common-sense systems, and emotional and
behavioral psychological systems.  Each of these impose sharply
conflicting requirements on the AtomSpace architecture; the AtomSpace
and "Atomese" is the current best-effort KR system for satisfying
all these various needs in an integrated way.  It is likely to
change, as the various current short-comings, design flaws,
performance and scalability issues are corrected.

### Atoms and Values
Currently, one of the primary conceptual and performance splits
are between "Atoms" and "Values". Atoms are:

* Used to represent graphs, networks, and long-term stable graphical relations.
* Indexed (by the AtomSpace) and enable the rapid search and traversal of graphs.
* Globally unique, and thus unambiguous anchor points for data.
* Immutable: can only be created and destroyed, and are effectively static and unchanging.
* Large, bulky, heavy-weight.

By contrast, Values, and valuations in general, are:
* A way of holding on to rapidly-changing data, including streaming data.
* Hold "truth values" and "probabilities", which change over time as new
  evidence is accumulated.
* Provide a per-Atom key-value store (noSQL database).
* Are not indexed, and are accessible only by direct reference.
* Small, fast, fleeting.

Thus, for example, a piece of knowledge, or some proposition would be
stored as an Atom.  As new evidence accumulates, the truth value of the
proposition is adjusted. Other fleeting changes, or general free-form
annotations can be stored as Values.  Essentially, the AtomSpace looks
like a database-of-databases; each atom is a key-value database; the
atoms are related to one-another as a graph.

### More info
The primary documentation for the atomspace and Atomese is here:

* https://wiki.opencog.org/w/AtomSpace
* https://wiki.opencog.org/w/Atomese
* https://wiki.opencog.org/w/Atom
* https://wiki.opencog.org/w/Value

The main project site is at https://opencog.org


New Developers; Pre-requisite skills
====================================
The AtomSpace is a relatively mature system, and thus fairly complex.
Because other users depend on it, it is not very "hackable"; it needs
to stay relatively stable.  Despite this, it is simultaneously a
research platform for discovering the proper way of adequately
representing knowledge in a way that is useful for general intelligence.
It turns out that knowledge representation is not easy.  This project
is a good place to explore it, if you're interested in that sort of thing.

Most developers should think of the AtomSpace as being kind-of-like an
operating system kernel, or the guts of a database: its complex, and
you don't need to know how the innards work to use the system. These
innards are best left to committed systems programmers and research
scientists; there is no easy way for junior programmers to participate,
at least, not without a lot of hard work and study.

Experience in any of the following areas will make things easier for
you; in fact, if you are good at any of these, please seriously consider
joining the project.

* Database internals; query optimization.
* Logic programming; Prolog.
* SAT-solving; Answer Set programming; Satisfiability Modulo Theories.
* Programming language design &amp; implementation.
* Rule engines; reasoning; inference; parsing.
* Theorem-proving systems; Type theory.
* Compiler internals; code generation; code optimization; bytecode; VM's.
* Operating systems; distributed database internals.

Basically, Atomese is a mash-up of ideas taken from all of the above fields.
It's kind-of trying to do and be all of these, all at once, and to find the
right balance between all of them. Again: the goal is knowledge representation
for general intelligence. Building something that the AGI developers can use.

If you don't have at least some fair grounding in one of the above,
you'll be lost, and find it hard to contribute.  If you do know something
about any of these topics, then please dive into the open bug list. Fixing
bugs is the #1 best way of learning the internals of a system.

Key Development Goals
=====================
Looking ahead, some key major projects.

### Distributed Processing
One of the major development goals for the 2019-2021 time frame
is to gain experience with distributed data processing. Currently,
the AtomSpace uses Postgres to provide distributed, scalable
storage. We're also talking about porting to Apache Ignite, or
possibly some other graph database, such as Redis, Riak or Grakn,
all of which also support scalable, distributed storage.

However, despite the fact that Postgres is already distributed,
and fairly scalable, none of the actual users of the AtomSpace
use it in it's distributed mode. Exactly why this is the case
remains unclear: is it the difficulty of managing a distributed
Postgres database? (I guess you have to be a good DB Admin to
know how to do this?) Is it the programming API offered by the
AtomSpace?  Maybe it's not yet urgent for them?  Would rebasing
on a non-SQL database (such as Ignite, Riak, Redis or Grakn) make
this easier and simpler?  This is quite unclear, and quite unknown
at this stage.

If a port to one of the distributed graph databases is undertaken,
there are several implementation issues that need to be cleared
up.  One is to eliminate many usages of SetLink (
[Issues #1502](https://github.com/opencog/atomspace/issues/1502)
and [#1507](https://github.com/opencog/atomspace/issues/1507) ).
Another is to change the AtomTable API to look like a bunch
of MemberLink's.  (Currently, the AtomTable conceptually looks and
behaves like a large set, which makes scaling and distribution
harder than it could be). How to transform the AtomTable into a bunch
of MemberLinks without blowing up RAM usage or hurting performance
is unclear.


### Exploring Values

The new Value system seems to provide a very nice way of working
with fast-moving high-frequency data.  It seems suitable for holding
on to live-video feeds and audio streams and piping them through
various data-processing configurations. It looks to be a decent
API for declaring the structure and topology of neural nets (e.g.
TensorFlow).  However, it is more-or-less unused for these tasks.
Apparently, there is still some missing infrastructure, as well as
some important design decisions to be made. Developers have not begun
to explore the depth and breadth of this subsystem, to exert pressure
on it.  Ratcheting up the tension by exploring new and better ways of
using and working with Values will be an important goal for the
2018-2022 timeframe.


### Sheaf theory

Many important types of real-world data, include parses of natural
language and biochemical processes resemble the abstract mathematical
concept of "sheaves", in the sense of sheaf theory.  One reason that
things like deep learning and neural nets work well is because some
kinds of sheaves look like tensor algebras; thus one has things like
Word2Vec and SkipGram models.  One reason why neural nets still
stumble on natural language processing is because natural language
only kind-of-ish, partly looks like a tensor algebra. But natural
language looks a whole lot more like a sheaf (because things like
pre-group grammars and categorial grammars "naturally" look like
sheaves.)  Thus, it seems promising to take the theory and all the
basic concepts of deep learning and neural nets, rip out the explicit
tensor-algebra in those theories, and replace them by sheaves. A
[crude sketch is here](/opencog/sheaf/docs/sheaves.pdf).

Some primitive, basic infrastructure has been built. Huge remaining
work items are using neural nets to perform the tensor-like factorization
of sheaves, and to redesign the rule-engine to use sheaf-type theorem
proving techniques.


Building and Installing
=======================
The Atomspace runs on more-or-less any flavor of GNU/Linux. It does not
run on any non-Linux operating systems (except maybe some of the BSD's).
Sorry!

There are a small number of pre-requisites that must be installed
before it can be built.  Many users will find it easiest to use the
install scripts provided in the [ocpkg repo](https://github.com/opencog/ocpkg).
Some users may find some success with one of the
[opencog Docker containers](https://github.com/opencog/docker).
Developers interested in working on the AtomSpace must be able to build
it manually. If you can't do that, all hope is lost.

### Prerequisites

To build the OpenCog AtomSpace, the packages listed below are required.
Essentially all Linux distributions will provide these packages.

###### boost
* C++ utilities package.
* https://www.boost.org/ | `apt-get install libboost-dev`

###### cmake
* Build management tool; v2.8 or higher recommended.
* https://www.cmake.org/ | `apt-get install cmake`

###### cogutil
* Common OpenCog C++ utilities.
* https://github.com/opencog/cogutil
* It uses exactly the same build procedure as this package. Be sure
  to `sudo make install` at the end.

###### guile
* Embedded scheme REPL (version 2.0.9 or newer is required; version 2.2
  is strongly recommended).
* https://www.gnu.org/software/guile/guile.html | `apt-get install guile-2.0-dev`

### Optional Prerequisites

The following packages are optional. If they are not installed, some
optional parts of the AtomSpace will not be built.  The CMake command,
during the build, will be more precise as to which parts will not be built.

###### cxxtest
* Test framework
* Optional but recommended; required for running unit tests.
* https://cxxtest.sourceforge.net/ | https://launchpad.net/~opencog-dev/+archive/ppa

###### Cython
* C bindings for Python.
* Strongly recommended, as many examples and important subsystems
  assume python bindings.
* https://cython.org | `apt-get install cython`

###### Haskell
* Haskell bindings (experimental).
* Optional; almost no existing code makes use of Haskell.
* https://www.haskell.org/

###### Postgres
* Distributed, multi-client networked storage.
* Needed for "remembering" things between shutdowns.
* https://postgres.org | `apt-get install postgresql postgresql-client libpq-dev`

###### ZeroMQ (version 3.2.4 or higher)
* Asynchronous messaging library.
* Optional, almost completely unused, mostly due to poor performance.
* https://zeromq.org/intro:get-the-software | `apt-get install libzmq3-dev`

###### Google Protocol Buffers
* Google's data interchange format (used by ZeroMQ).
* Optional, needed only for ZMQ, above.
* https://developers.google.com/protocol-buffers | `apt-get install libprotobuf-dev`


### Building AtomSpace

Be sure to install the pre-requisites first!
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


### Unit tests

To build and run the unit tests, from the `./build` directory enter
(after building opencog as above):
```
    make test
```

### Install

After building, you MUST install the atomspace.
```
    sudo make install
```

Writing Atomese
===============
Atomese -- that is -- all of the different Atom types, can be thought
of as the primary API to the AtomSpace.  Atoms can, of course, be
created and manipulated with Atomese; but, in practice, programmers
will work with either scheme (guile), python, C++ or haskell.

The simplest, most complete and extensive interface to Atoms and the
Atomspace is via scheme, and specifically, the GNU Guile scheme
implementation.  An extensive set of examples can be found in the
[`/examples/atomspace`](/examples/atomspace) and the
[`/examples/pattern-matcher`](/examples/pattern-matcher) directories.

Python is more familiar than scheme to most programmers, and it offers
another way of interfacing to the atomspace. Unfortunately, it is not
as easy and simple to use as scheme; it also has various technical issues.
Thus it is significantly less-used than scheme in the OpenCog project.
None-the-less, it remains vital for various applications. See the
[`/examples/python`](/examples/python) directory for how to use python
with the AtomSpace.
