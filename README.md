OpenCog AtomSpace
=================

[![CircleCI](https://circleci.com/gh/opencog/atomspace.svg?style=svg)](https://circleci.com/gh/opencog/atomspace)

The OpenCog AtomSpace is an in-RAM knowledge representation (KR)
database, an associated query engine and graph-re-writing system,
and a rule-driven inferencing engine that can apply and manipulate
sequences of rules to perform reasoning. It is a kind of in-RAM
generalized hypergraph (metagraph) database. Metagraphs offer more
efficient, more flexible and more powerful ways of representing
graphs: [a metagraph store is literally just-plain better than a
graph store.](https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/ram-cpu.pdf)
On top of this, the Atomspace provides a large variety of advanced
features not available anywhere else.

The AtomSpace is a platform for building Artificial General Intelligence
(AGI) systems. It provides the central knowledge representation component
for OpenCog. As such, it is a fairly mature component, on which a lot of
other systems are built, and which depend on it for stable, correct
operation in a day-to-day production environment.

Data as MetaGraphs
==================
It is now commonplace to represent data as graphs; there are more graph
databases than you can shake a stick at. What makes the AtomSpace
different? A dozen features that no other graph DB does, or has even
dreamed of doing.

But, first: five things everyone else does:
* Perform [graphical database queries](https://wiki.opencog.org/w/Pattern_engine),
  returning results that satisfy a provided search pattern.
* Arbitrarily complex patterns with an arbitrary number of variable
  regions can be specified, by unifying multiple clauses.
* Modify searches with conditionals, such as "greater than", and with
  user callbacks into scheme, python or Haskell.
* Perform graph rewriting: use search results to create new graphs.
* Trigger execution of user callbacks... or of executable graphs (as
  explained below).

A key difference: the AtomSpace is a metagraph store, not a graph store.
Metagraphs can efficiently represent graphs, but not the other way around.
This is carefully explained
[here,](https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/ram-cpu.pdf)
which also gives a precise definition of what a metagraph is, and how it
is related to a graph.  As a side-effect, metagraphs open up many
possibilities not available to ordinary graph databasases. These are
listed below.  Things are things that no one else does:
* **Search queries are graphs.**
  (The API to the [pattern engine](https://wiki.opencog.org/w/Pattern_engine)
  is a graph.) That is, every query, every search is also a graph. That
  means one can store a collection of searches in the database, and
  access them later. This allows a graph rule engine to be built up.
* **Inverted searches.**
  ([DualLink](https://wiki.opencog.org/w/DualLink).)
  Normally, a search is like "asking a question" and "getting an
  answer". For the inverted search, one "has an answer" and is looking
  for all "questions" for which its a solution. This is pattern
  recognition, as opposed to pattern search. All chatbots do this as
  a matter of course, to handle chat dialog. No chatbot can host
  arbitrary graph data, or search it. The AtomSpace can. This is because
  queries are also graphs, and not just data.
* Both [**"meet" and "join"**](https://en.wikipedia.org/wiki/Join_and_meet)
  searches are possible: One can perform a "fill in the blanks" search
  (a meet, with [MeetLink](https://wiki.opencog.org/w/MeetLink))
  and one can perform a "what contains this?" search (a join, with
  [JoinLink](https://wiki.opencog.org/w/JoinLink).)
* **Graphs are executable.** Graph vertex types include "plus", "times",
  "greater than" and many other programming constructs. The resulting
  graphs encode
  ["abstract syntax trees"](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
  and the resulting language is called
  [Atomese](https://wiki.opencog.org/w/Atomese).
  It resembles the
  [intermediate representation](https://en.wikipedia.org/wiki/Intermediate_representation)
  commonly found in compilers, except that, here, its explicitly exposed
  to the user as a storable, queriable, manipulable, executable graph.
* **Graphs are typed**
  ([TypeNode](https://wiki.opencog.org/w/TypeNode) and
  [type constructors](https://wiki.opencog.org/w/Type_constructor).)
  Graph elements have types, and there are half a dozen type
  constructors, including types for graphs that are functions. This
  resembles programming systems that have type constructors, such as
  CaML or Haskell.
* **Graphs specify flows**
  ([Values](https://wiki.opencog.org/w/Value) and
  [DynamicFormulaLink](https://wiki.opencog.org/w/DynamicFormulaLink).)
  Graph elements host dynamic, mutable
  key-value databases. That is, every graph element has an associated
  key-value database. Think of the graph is "pipes" or "plumbing"; the
  key-value data is the mutable, dynamically changing "water" that flows
  through those pipes.
* **Unordered sets**
  ([UnorderedLink](https://wiki.opencog.org/w/UnorderedLink).)
  A graph vertex can be an unordered set (Think of a list of edges, but
  they are not in any fixed order.) When searching for a matching
  pattern, one must consider **all** permutations of the set. This is
  easy, if the search has only one unordered set. This is hard, if
  they are nested and inter-linked: it becomes a constraint-satisfaction
  problem.  The AtomSpace pattern engine handles all of these cases
  correctly.
* **Alternative sub-patterns**
  ([ChoiceLink](https://wiki.opencog.org/w/ChoiceLink).)
  A search query can include a menu of sub-patterns to be matched. Such
  sets of alternatives can be nested and composed arbitrarily. (*i.e.*
  they can contain variables, *etc.*)
* **Globby matching**
  ([GlobNode](https://wiki.opencog.org/w/GlobNode).)
  One can match zero, one or more subgraphs with globs This is similar
  to the idea of globbing in a regex. Thus, a variable need not be
  grounded by only one subgraph: a variable can be grounded by an
  indeterminate range of subgraphs.
* **Quotations** ([QuoteLink](https://wiki.opencog.org/w/QuoteLink).)
  Executable graphs can be quoted.  This is similar to quotations in
  functional programming languages. In this case, it allows queries
  to search for other queries, without triggering the query that was
  searched for. Handy for rule-engines that use rules to find other
  rules.
* **Negation as failure**
  ([AbsentLink](https://wiki.opencog.org/w/AbsentLink).)
  Reject matches to subgraphs having particular sub-patterns in them.
  That is, find all graphs of some shape, except those having these
  other sub-shapes.
* **For-all predicate**
  ([AlwaysLink](https://wiki.opencog.org/w/AlwaysLink).)
  Require that all matches contain a particular subgraph or satisfy a
  particular predicate.  For example: find all baskets that have only
  red balls in them. This requires not only finding the baskets, making
  sure they have balls in them, but also testing each and every ball in
  a basket to make sure they are **all** of the same color.

### What it isn't
Newcomers often struggle with the AtomSpace, because they bring
preconceived notions of what they think it should be, and its not that.
So, a few things it is not.

* **Its not JSON.**  So JSON is a perfectly good way of representing
  structured data. JSON records data as `key:value` pairs, arranged
  hierarchically, with braces, or as lists, with square brackets.
  The AtomSpace is imilar, except that there are not keys! The
  AtomSpace still organizes data hierarchically, and provides lists,
  but all entries are anonymous, nameless. Why? There are CPU performance,
  RAM usage and design tradeoffs in not using explict named keys in the
  data structure. You can still have named values; it is just that they
  are not required. There are several different ways of importing JSON
  data into the AtomSpace. If your mental model of "data" is JSON, then
  you will be confused by the AtomSpace.

* **It's not SQL. It's also not noSQL**. Databases from 50 years ago
  organized structured data into tables, where the `key` is the label
  of a column, and different `values` sit in different rows. This is
  more efficient than JSON, if you have many rows: you don't have to
  store the same key over and over again, for each row. Of course,
  tabular data is impractical if you have zillions of tales, each with
  only one or two rows. That's one reason why JSON was invented.
  The AtmSpace was designed to store *unstructured* data. You can
  still store structured data in it; there are several different ways
  of importing tabular data into the AtomSpace. If your mental model
  of "data" is structured data, then you will be confused by the AtomSpace.

* **It's not a vertex+edge store**. (Almost?) all graph databases
  decompose graphs into lists of vertexes and edges. This is just fine,
  if you don't use complex algorithms. The problem with this storage
  format is locality: graph traversal becomes a game of repeatedly
  looking up a sepcific vertex and then, a sepcific edge, each taken
  from a large table for vertexes and edges. This is non-local, it
  requires large indexes on those tables (requires a lot of RAM),
  and the lookups are CPU consuming. Graph traversal can be a
  bottleneck. The AtomSpace avoids much of this overhead by using
  (hyper-/meta-)graphs. This enables more effective and simpler
  traversal algorithms, which in turn allows more sophisticated
  search features to be implemented.  If your mental model of
  graph data is lists of vertexes and edges, then you will be confused
  by the AtomSpace.

The actual AtomSpace resembles some aspects of all three, without
being specifically any of them. It tries to be general: it wants to
let you work with structured data or with unstructured data or with
graphs, however you please. It does not force any particular style.
Its meant to be general.

That said: it means that the AtomSpace is different and unusual.
It might be a bit outside of the comfort zone for most programmers.
It doesn't have API's that are instantly recognizable to users of
these other systems. There is a challenging learning curve involved,
here. We're sorry about that: if you have ideas for better API's that
would allow the AtomSpace to look more conventional, then contact us!

### Status and Invitation

As it turns out, knowledge representation is hard, and so the AtomSpace
has been (and continues to be) a platform for active scientific research
on knowledge representation, knowledge discovery and knowledge
manipulation.  If you are comfortable with extremely complex
mathematical theory, and just also happen to be extremely comfortable
writing code, you are invited -- encouraged -- to join the project.


Using Atomese and the AtomSpace
===============================
The AtomSpace is not an "app". Rather, it is a knowledge-base platform.
It is probably easiest to think of it as kind-of-like an operating
system kernel: you don't need to know how it works to use it. You
probably don't need to tinker with it. It just works, and it's there
when you need it.

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
* [Unsupervised natural language learning](https://github.com/opencog/learn)
  (learn repo)
* [Genomic, proteomic data analysis](https://github.com/opencog/agi-bio)
  (agi-bio repo) and various [MOZI.AI](https://github.com/mozi-ai) repos.
* [Opencog on a Raspberry Pi](https://github.com/opencog/tinycog)
  (tinycog repo)
* [Port of the MOSES machine learning to Atomese](https://github.com/opencog/as-moses)
  (as-moses repo)


Examples, Documentation, Blog
=============================
If you are impatient, a good way to learn the AtomSpace is to run the
example demos. [Start with these.](examples/atomspace) Then move on to
the [pattern-matcher examples](examples/pattern-matcher).

Documentation is on the OpenCog wiki. Good places to start are here:
* [AtomSpace](https://wiki.opencog.org/w/AtomSpace)
* [Atom types](https://wiki.opencog.org/w/Atom_types)
* [Pattern matching](https://wiki.opencog.org/w/Pattern_matching)

The [OpenCog Brainwave blog](https://blog.opencog.org/) provides reading
material for what this is all about, and why.

A Theoretical Overview
======================
The AtomSpace is a mashup of a large variety of concepts from
mathematical logic, theorem proving, graph theory, database theory,
type theory, model theory and knowledge representation. Its hard to
provide a coherent overview without throwing around a lot of "big words"
and "big concepts".  We're trying to get a lot of things done, here,
and there's no particularly simple or effective way of explaining it
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
construction. See [Atom types](https://wiki.opencog.org/w/Atom_types).

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
See [Atomese](https://wiki.opencog.org/w/Atomese).

Aside from the various advanced features, Atomese also has some very
basic and familiar atom types: atoms for arithmetic operations like
"plus" and "times", conditional operators, like "greater-than" or
"equals", control operations like "sequential and" and "cond", as
well as settable state. This makes Atomese resemble a kind of
intermediate language, something you might find inside of a compiler,
a bit like CIL or Gimple. However, it is both far more flexible and
powerful than these, and also far less efficient. Adventurous souls
are invited to create a compiler to GNU Lighting, CIL, Java bytecode
or the bytecode of your choice; or maybe to a GPU backend, or even
more complex data-processing systems, such as TensorFlow.

In its current form, Atomese was primarily designed to allow the
generalized manipulation of large networks of probabilistic data by
means of rules and inferences and reasoning systems.  It extends the
idea of probabilistic logic networks to a generalized system for
algorithmically manipulating and managing data. The current, actual
design has been heavily influenced by practical experience with
natural-language processing, question answering, inferencing and
the specific needs of robot control.

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

Active researchers and theoreticians are invited to join! The current
codebase is *finally* clean and well-organized enough that a large
number of possibilities have opened up, offering many different and
exciting directions to pursue. The system is clean and flexible, and
ready to move up to the next level.

### Atoms and Values
One of the primary conceptual distinctions in Atomese is between
"Atoms" and "Values". The distinction is made for both usability and
performance.  Atoms are:

* Used to represent graphs, networks, and long-term stable graphical relations.
* Indexed (by the AtomSpace), which enables the rapid search and traversal of graphs.
* Globally unique, and thus unambiguous anchor points for data.
* Immutable: can only be created and destroyed, and are effectively static and unchanging.
* Large, bulky, heavy-weight (because indexes are necessarily bulky).

By contrast, Values, and valuations in general, are:
* A way of holding on to rapidly-changing data, including streaming data.
* Hold "truth values" and "probabilities", which change over time as new
  evidence is accumulated.
* Provide a per-Atom key-value store (a mini noSQL database per-Atom).
* Are not indexed, and are accessible only by direct reference.
* Small, fast, fleeting (no indexes!)

Thus, for example, a piece of knowledge, or some proposition would be
stored as an Atom.  As new evidence accumulates, the truth value of the
proposition is adjusted. Other fleeting changes, or general free-form
annotations can be stored as Values.  Essentially, the AtomSpace looks
like a database-of-databases; each atom is a key-value database; the
atoms are related to one-another as a graph. The graph is searchable,
editable; it holds rules and relations and ontologies and axioms.
Values are the data that stream and flow through this network, like
water through pipes. Atoms define the pipes, the connectivity. Values
flow and change. See the blog entry
[value flows](https://blog.opencog.org/2020/04/08/value-flows/) as
well as [Atom](https://wiki.opencog.org/w/Atom) and
[Value](https://wiki.opencog.org/w/Value).

### More info
The primary documentation for the atomspace and Atomese is here:

* https://wiki.opencog.org/w/AtomSpace
* https://wiki.opencog.org/w/Atomese
* https://wiki.opencog.org/w/Atom
* https://wiki.opencog.org/w/Value

The main project site is at https://opencog.org


New Developers; Pre-requisite skills
====================================
Most users should almost surely focus their attention on one of the
high-level systems built on top of the AtomSpace. The rest of this
section is aimed at anyone who wants to work *inside* of the AtomSpace.

Most users/developers should think of the AtomSpace as being kind-of-like
an operating system kernel, or the guts of a database: its complex, and
you don't need to know how the innards work to use the system. These
innards are best left to committed systems programmers and research
scientists; there is no easy way for junior programmers to participate,
at least, not without a lot of hard work and study.  Its incredibly
exciting, though, if you know what you're doing.

The AtomSpace is a relatively mature system, and thus fairly complex.
Because other users depend on it, it is not very "hackable"; it needs
to stay relatively stable.  Despite this, it is simultaneously a
research platform for discovering the proper way of adequately
representing knowledge in a way that is useful for general intelligence.
It turns out that knowledge representation is not easy.  This project
is a -good- excellent place to explore it, if you're interested in that
sort of thing.

Experience in any of the following areas will make things easier for
you; in fact, if you are good at any of these ... we want you. Bad.

* Database internals; query optimization.
* Logic programming; Prolog.
* SAT-solving; Answer Set programming; Satisfiability Modulo Theories.
* Programming language design &amp; implementation.
* Rule engines; reasoning; inference; parsing.
* Theorem-proving systems; Type theory.
* Compiler internals; code generation; code optimization; bytecode; VM's.
* Operating systems; distributed database internals.
* GPU processing pipelines, lighting-shading pipelines, CUDA, OpenCL.
* Dataflow in GPU's for neural networks.

Basically, Atomese is a mash-up of ideas taken from all of the above
fields.  It's kind-of trying to do and be all of these, all at once,
and to find the right balance between all of them. Again: the goal is
knowledge representation for general intelligence. Building something
that the AGI developers can use.

We've gotten quite far; we've got a good, clean code-base, more-or-less,
and we're ready to kick it to the next level. The above gives a hint of
the directions that are now open and ready to be explored.

If you don't have at least some fair grounding in one of the above,
you'll be lost, and find it hard to contribute.  If you do know something
about any of these topics, then please dive into the open bug list. Fixing
bugs is the #1 best way of learning the internals of any system.

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
2020-2024 time-frame. See the
[value flows](https://blog.opencog.org/2020/04/08/value-flows/) blog
entry.


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
of sheaves, and to redesign the rule engine to use sheaf-type theorem
proving techniques.

Current work is split between two locations: the "sheaf" subdirectory
in this repo, and the [generate](https://github.com/opencog/generate)
repo.


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
* Build management tool; v3.0.2 or higher recommended.
* https://www.cmake.org/ | `apt-get install cmake3`

###### cogutil
* Common OpenCog C++ utilities.
* https://github.com/opencog/cogutil
* It uses exactly the same build procedure as this package. Be sure
  to `sudo make install` at the end.

###### guile
* Embedded scheme REPL (version 2.2.2 or newer required, 3.0 preferred.)
* https://www.gnu.org/software/guile/guile.html
* For Debian/Ubuntu,  `apt-get install guile-2.2-dev`

###### cxxtest
* Unit test framework
* Required for running unit tests. Breaking unit tests is verboten!
* https://cxxtest.com/ | `apt-get install cxxtest`

### Optional Prerequisites

The following packages are optional. If they are not installed, some
optional parts of the AtomSpace will not be built.  The `cmake` command,
during the build, will be more precise as to which parts will not be built.

###### Cython
* C bindings for Python. (version 0.23 or newer)
* Recommended, as many users enjoy using python.
* https://cython.org | `apt-get install cython`

###### Haskell
* Haskell bindings (experimental).
* Optional; almost no existing code makes use of Haskell.
* https://www.haskell.org/

###### Postgres
* Distributed, multi-client networked storage.
* Needed for "remembering" between shutdowns (and for distributed AtomSpace)
* https://postgres.org | `apt-get install postgresql postgresql-client libpq-dev`

### Building AtomSpace

Be sure to install the pre-requisites first!
Perform the following steps at the shell prompt:
```
    cd to project root dir
    mkdir build
    cd build
    cmake ..
    make -j4
    sudo make install
    make -j4 test
```
Libraries will be built into subdirectories within build, mirroring
the structure of the source directory root.


### Unit tests

To build and run the unit tests, from the `./build` directory enter
(after building opencog as above):
```
    make -j4 test
```
Most tests (just not the database tests) can be run in parallel:
```
    make -j4 test ARGS=-j4
```
The database tests *will* fail if run in parallel: they will step on
one-another, since they all set and clear the same database tables.

Specific subsets of the unit tests can be run:
```
    make test_atomese
    make test_atomspace
    make test_guile
    make test_join
    make test_matrix
    make test_persist_sql
    make test_python
    make test_query
    make test_sheaf
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
Thus, it is significantly less-used than scheme in the OpenCog project.
None-the-less, it remains vital for various applications. See the
[`/examples/python`](/examples/python) directory for how to use python
with the AtomSpace.
