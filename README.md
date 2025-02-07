OpenCog AtomSpace
=================

[![CircleCI](https://circleci.com/gh/opencog/atomspace.svg?style=svg)](https://circleci.com/gh/opencog/atomspace)

The OpenCog AtomSpace is an in-RAM knowledge representation (KR)
database with an associated query engine and graph-re-writing system.
It is a kind of in-RAM generalized hypergraph (metagraph) database.
Metagraphs offer more efficient, more flexible and more powerful ways
of representing graphs: [a metagraph store is literally just-plain
better than a graph store.](https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/ram-cpu.pdf)
On top of this, the Atomspace provides a large variety of advanced
features not available anywhere else.

The AtomSpace is a platform for building Artificial General Intelligence
(AGI) systems. It provides the central knowledge representation component
for OpenCog. As such, it is a fairly mature component, on which a lot of
other systems are built, and which depend on it for stable, correct
operation in a day-to-day production environment.

There are several dozen modules built on top of the AtomSpace. Notable
ones include:

* [Store AtomSpaces to disk](https://github.com/opencog/atomspace-rocks)
* [Network-distributed AtomSpace storage](https://github.com/opencog/atomspace-cog)
* [Network shell to AtomSpaces, including a WebSocket API](https://github.com/opencog/cogserver)
* [Sparse Vector/Matrix embeddings/access to graphs](https://github.com/opencog/matrix)
* [Sensori-motor research](https://github.com/opencog/sensory)
* [Language learning](https://github.com/opencog/learn)

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
* Perform **graph rewriting**: use search results to create new graphs.
* Trigger execution of user callbacks... or of executable graphs (as
  explained below).

A key difference: the AtomSpace is a metagraph store, not a graph store.
Metagraphs can efficiently represent graphs, but not the other way around.
This is carefully explained
[here,](https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/ram-cpu.pdf)
which also gives a precise definition of what a metagraph is, and how it
is related to a graph.  As a side-effect, metagraphs open up many
possibilities not available to ordinary graph databases. These are
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
* **Graph nodes carry vectors**
  [Values](https://wiki.opencog.org/w/Value) are mutable vectors of
  data. Each graph element (vertex or edge, node or link) can host
  an arbitrary collection of Values. This is, each graph element is
  also a key-value database.
* **Graphs specify flows**
  Values can be static or dynamic.  For the dynamic case, a given
  graph can be thought of as "pipes" or "plumbing"; the Values can
  "flow" along that graph.  For example, the
  [FormulaStream](https://wiki.opencog.org/w/FormulaStream) allows
  numeric vector operations ("formulas") to be defined. Accessing
  a FormulaStream provides the vector value *at that instant*.
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
* **Frames (ChangeSets)**
  Store a sequence of graph rewrites, changes of values as a single
  changeset. The database itself is a collection of such changesets or
  "Frames".  Very roughly, a changeset resembles a git commit, but for
  the graph database. The word "Frame" is mean to invoke the idea of a
  stackframe, or a Kripke frame: the graph state, at this moment. By
  storing frames, it is possible to revert to earlier graph state. It is
  possible to compare different branches and to explore different
  rewrite histories starting from the same base graph.  Different
  branches may be merged, forming a set-union of thier contents. This
  is useful for inference and learning algos, which explore long chains
  of large, complex graph rewrites.


### What it Isn't
Newcomers often struggle with the AtomSpace, because they bring
preconceived notions of what they think it should be, and its not that.
So, a few things it is not.

* **It's not JSON.**  So JSON is a perfectly good way of representing
  structured data. JSON records data as `key:value` pairs, arranged
  hierarchically, with braces, or as lists, with square brackets.
  The AtomSpace is similar, except that there are no keys! The
  AtomSpace still organizes data hierarchically, and provides lists,
  but all entries are anonymous, nameless. Why? There are performance
  (CPU and RAM usage) and other design benefits in not using explicit
  named keys in the data structure. You can still have named values;
  it is just that they are not required. There are several different
  ways of importing JSON data into the AtomSpace. If your mental model
  of "data" is JSON, then you will be confused by the AtomSpace.

* **It's not SQL. It's also not noSQL**. Databases from 50 years ago
  organized structured data into tables, where the `key` is the label
  of a column, and different `values` sit in different rows. This is
  more efficient than JSON, when you have many rows: you don't have to
  store the same key over and over again, for each row. Of course,
  tabular data is impractical if you have zillions of tables, each with
  only one or two rows. That's one reason why JSON was invented.
  The AtomSpace was designed to store *unstructured* data. You can
  still store structured data in it; there are several different ways
  of importing tabular data into the AtomSpace. If your mental model
  of "data" is structured data, then you will be confused by the AtomSpace.

* **It's not a vertex+edge store**. (Almost?) all graph databases
  decompose graphs into lists of vertexes and edges. This is just fine,
  if you don't use complex algorithms. The problem with this storage
  format is locality: graph traversal becomes a game of repeatedly
  looking up a specific vertex and then, a specific edge, each located
  in a large table of vertexes and edges. This is non-local; it
  requires large indexes on those tables (requires a lot of RAM),
  and the lookups are CPU consuming. Graph traversal can be a
  bottleneck. The AtomSpace [avoids much of this overhead by using
  (hyper-/meta-)graphs.](https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/ram-cpu.pdf)
  This enables more effective and simpler
  traversal algorithms, which in turn allows more sophisticated
  search features to be implemented.  If your mental model of
  graph data is lists of vertexes and edges, then you will be confused
  by the AtomSpace.


**What is it, then?** Most simply, the AtomSpace stores immutable,
globally unique, [typed](https://en.wikipedia.org/wiki/Type_theory)
[s-expressions.](https://en.wikipedia.org/wiki/S-expression) The types
can be thought of as being like object-oriented classes, and many (not
all) Atom types do have a corresponding C++ class. Each s-expression is
called "an Atom". Each Atom is globally unique: there is only one copy,
ever, of any given s-expression (Atom). It's almost just that simple,
with only one little twist: a (mutable) key-value database is attached
to each Atom. Now, "ordinary" graph databases do this too: every vertex
or edge can have "attributes" on it. The AtomSpace allows these
attributes to be dynamic: to change in time or to "flow". The flow
itself is described by a graph; thus, graphs can be thought of as
"plumbing"; whereas the Values are like the "fluid" in these pipes.
This is much like the distinction between "software" and "data":
software describes algos, data is what moves through them. In the
AtomSpace, the algos are explicit graphs. The Values are the data.

The AtomSpace borrows ideas and concepts from many different systems,
including ideas from JSON, SQL and graph stores. The goal of the
AtomSpace is to be general: to allow you to work with whatever style
of data you want: structured or unstructured. As graphs, as tables,
as objects. As lambda expressions, as abstract syntax trees, as
prolog-like logical statements.  A place to store relational data
obeying some relational algebra. As a place to store ontologies or
mereologies or taxonomies. A place for syntactic (BNF-style)
productions or constraints or RDF/OWL-style schemas.
In a mix of declarative, procedural and functional styles.
The AtomSpace is meant to allow general knowledge representation,
in any format.

The "special extra twist" of immutable graphs decorated with mutable
values resembles a corresponding idea in logic: the split between
logical statements, and the truth values (valuations) attached to them.
This is useful not only for logic, but also for specifying data
processing pipelines: the graph specifies the pipeline; the values are
what flow through that pipeline. The graph is the "code"; the values
are the data that the code acts on.

All this means that the AtomSpace is different and unusual.
It might be a bit outside of the comfort zone for most programmers.
It doesn't have API's that are instantly recognizable to users of
these other systems. There is a challenging learning curve involved.
We're sorry about that: if you have ideas for better API's that
would allow the AtomSpace to look more conventional, and be less
intimidating to most programmers, then contact us!

### Status and Invitation

As it turns out, knowledge representation is hard, and so the AtomSpace
has been (and continues to be) a platform for active scientific research
on knowledge representation, knowledge discovery and knowledge
manipulation.  If you are comfortable with extremely complex
mathematical theory, and just also happen to be extremely comfortable
writing code, you are invited -- encouraged -- to join the project.

### Related ideas
A short list of some related concepts:

* [Carnegie Mellon Binary Analysis Platforrm (BAP)](https://github.com/BinaryAnalysisPlatform/bap)
  allows binary programs (viruses, etc.) to be disassembled and analyzed.
  The disassembled program is stored as a graph in a database. The graph
  can be analyzed, investigated, and even executed, to see what it does.
  Thus, similar to the AtomSpace, but very highly specialized for binaries,
  and nothing else.

* [Modelica](https://en.wikipedia.org/wiki/Modelica) is a modelling
  language for describing complex systems. Intended for describing
  mechanical, electrical, electronic, hydraulic, thermal, control,
  electric power and process-oriented systems. The descriptions are
  static, object-oriented, file-based, and meant to be written by
  humans. That is, the models are atomated, but not the creation and
  management of them. Not suitable for general graph structures.

* The concept of graph programming.


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

* [Unsupervised natural language learning](https://github.com/opencog/learn)
  (learn repo)
* [JSON, Python, Scheme network interfaces](https://github.com/opencog/cogserver)
  (cogserver repo)

Zombie projects: these are half-dead; no one is currently working on them,
but they should still work and still provide useful capabilities.
* [Genomic, proteomic data analysis](https://github.com/opencog/agi-bio)
  (agi-bio repo) and various [MOZI.AI](https://github.com/mozi-ai) repos.
* [Port of the MOSES machine learning to Atomese](https://github.com/opencog/as-moses)
  (as-moses repo)
* [Unified Rule Engine](https://github.com/opencog/ure) (ure repo)
* [OpenAI Gym and Minecraft agents](https://github.com/opencog/rocca)
  (rocca repo)

Dead projects: these are no longer maintained. They used to work, but have
been abandoned for various theoretical and political reasons:
* [Natural language chat, robot control](https://github.com/opencog/opencog)
  (the opencog repo)
* [ROS bridge to robots, vision subsystem, chat](https://github.com/opencog/ghost_bridge)
  (ghost-bridge repo)
* [Opencog on a Raspberry Pi](https://github.com/opencog/tinycog)
  (tinycog repo)
* [Probabilistic Logic Networks](https://github.com/opencog/pln) (pln repo)


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
Atomese is a collection of structural primitives meant to describe
structural relationships as they are witnessed in "reality". This
includes descriptions of physical nature, biological nature,
psychological, social, cultural, political and economic, and, of course,
mathematical and technological. So, software and programming.

### Motivation

The idea of representing "everything" is as old as Aristotle. Set theory
is an early mathematical framework. This is followed by combinators and
lambda calulus, by means of which "anything sayable can be said". Modern
math offers Category Theory and Topos Theory, along with Proof Theory
and Model Theory as ways of talking about "anything". The goals of
mathematicians, however, are not the same as the more
entrepreneurial-minded, and the latter have created the trillion dollar
computer industry, with only token acknowledgement of the mathematical
foundations. The computer industry gives us relational databases,
knowledge representation, upper ontologies, and now LLM's, transformers
and weights as mechanisms by which "anything" can be represented.

Atomese is an ongoing attempt to roll all of this up into one, and to do
so in a way that makes general intelligence algorithmically accessible.
Until now, all attempts to extract structure from the universe are
complex systems hand-crafted by human engineers. These might be
financial credit-worthiness rating systems, or astronomical
stellar-redshift analysis tools. The software for these systems are
written by humans, applying conventional software development
methodologies, using conventional programming languages, designed to
make it easy for the human software engineer to perform their task.

What if, instead, we ask: what would it take to make it easy for
algorithmic systems to automatically explore and extract structure? To
create world-models that can be stored in short-term or long-term
memory, to process and transform sensory information, to drive motors
and perform actions in the real world? That is, rather than having a
small army of humans hand-crafting custom robots for others to use, to
instead provide a recursive infrastructure to allow, umm, err, robots to
craft themselves? This is the driving vision of Atomese.

### History

Atomese originally arose as an attempt by Ben Goertzel and company to
combine symbolic AI methods with probability theory, resulting in the
definition of PLN, Probabilistic Logic Networks, articulated in several
books devoted to the topic. In this articulation, the primitives of
knowledge representation theory are mashed up with mathematical logic to
provide Nodes and Links, which are general enough to represent almost
any kind of relational structure. The base object then becomes a
collection of graphs, or, more properly, hypergraphs. To be able to
process, digest, reason and manipulate these, these are placed in a
(hyper-)graph database, the AtomSpace.

To layer on probability theory onto what is otherwise a purely symbolic
representation of nature, the SimpleTruthValue is introduced. This is a
pair of floating-point numbers, representing the probability, and the
confidence of any given symbolic factual assertion. The goal is to
support logical reasoning systems of any type, not only conventional
Bayesian inference, but any collection of rule systems and axioms, as
might be encountered in mathematical proof theory. This would include,
for example, any of the rich varieties of modal logic, but also fuzzy
logic, the so-called "non-axiomatic reasoning systems" and
statistical-mechanical systems like Markov logic.

The word-phrase TruthValue, and more generally Value, has its roots in
mathematical logic, where any given assertion in first-order logic (or
higher-order logic) can be assigned a "valuation", indicating it's
binary truth/falsehood. Probability theory forces a replacement of crisp
0/1 by a floating-point number. Probabilistic logic (along with neural
nets) famously has issues with converging rapidly enough to a given
solution. For this reason, an extra float is introduced, the
"confidence". This helps, but is still not enough to capture the concept
of an ensemble, e.g. a "Bell curve", a Gaussian, or more generally any
kind of probability distribution: a "histogram" or more simply "a vector
of numbers". This leads to the idea of a FloatValue, and then rapidly to
a Value in general, which is a vector of anything at all, representing
truths in any ensemble, hypothetical modal universe, a set of Bayesian
priors, as the case may be. Of course, vectors of floats are the
bread-n-butter of neural nets.

Parallel universes, such as the hypothetical worlds of modal logic,
thermodynamic canonical ensembles, the infinite collection of Bayesian
priors, or, god forbid, quantum-mechanical decompositions, are often
imagined to live "in parallel" or to somehow co-exist temporally. In
physical reality, though, the changing network of relationships and
likelihoods is time-varying, and usually accessible only through sensory
devices, rather than through pure reason. This motivates the recasting
of Values as streams that flow data. This relegates the AtomSpace to
being a form of memory, a repository for world-models, while flowing
streams encapsulate the process of, well, "processing information". This
fits well with present-day software theory, which includes descriptions
of generators, futures and promises as software primitive constructs for
creating sensory agentic systems. The backends of large commercial
websites use futures and promises as extremely low-level programming
constructs to implement millisecond reaction times when customers click
on their favorite TikTok influencer. The point of having streams in
Atomese is not to be hopelessly abstract, but to capture an idea that is
already widespread in the design and development of agentic software
systems.

This brings Atomese to it's present-day state: an infrastructure for
symbolic AI, together with a (hyper-)graph database, offering dynamic
sensori-motor processing primitives. The hope is that this is an
appropriate toolset for agentic systems that can reify, transform and
transmute their own content. It remains a research platform to figure
out how this is possible, or, perhaps being more honest, if this is
possible.

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
One of the development goals for the 2021-2023 time frame
is to gain experience with distributed data processing. Currently,
one can build simple distributed networks of AtomSpaces, by using
the [**StorageNode**](https://wiki.opencog.org/w/StorageNode) to
specify a remote AtomSpace. However, it is up to you as to what
kinds of data these AtomSpace exchange with one-another. Only two
simple pre-configured communications styles have been created: the
read-thru and the write-thru proxies for the cogserver. These pass
incoming data and results on to the next nodes in the network.

### Cross-system Bridges
Because the AtomSpace can hold many different representatioinal
styles, it is relatively easy to import data into the AtomSpace.
The low-brow way to do this is to write a script file that imports
the data. This is fine, but leads to data management issues: who's
got the master copy?

The goal of data bridges is to create new Atoms that allow live
access into other online systems. For example, if an SQL database
holds a table of `(name, address, phone-number)`, it should be
possible to map this into the AtomSpace, such that updates not
only alter the SQL table, live and on line, but also such that
a query performed on the AtomSpace side translates into a query on
the SQL database side. This is not hard to do, but no one's done it
yet.

Similarly, a live online bridge between the AtomSpace and popular
graph databases should also be possible. It's not clear if this
should use the [StorageNode](https://wiki.opencog.org/w/StorageNode)
API mentioned above, or if it needs something else.


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
2021-2024 time-frame. See the
[value flows](https://blog.opencog.org/2020/04/08/value-flows/) blog
entry.

A particularly important first step would be to build interfaces
between values and an audio DSP framework. This would allow AtomSpace
structures to control audio processing, thus enabling (for example)
sound recognition (do I hear clapping? Cheers? Boos?) without having
to hard-code a "cheer recognizer". This opens the door to using machine
learning to learn how to detect different kinds of audio events.

There is no particular need to limit oneself to audio: other kinds
of data is possible (*e.g.* exploring the syntactic, hierarchical
part-whole structure in images) but audio is perhaps easier!?


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
* https://www.boost.org/ | `apt install libboost-dev`

###### cmake
* Build management tool; v3.0.2 or higher recommended.
* https://www.cmake.org/ | `apt install cmake`

###### cogutil
* Common OpenCog C++ utilities.
* https://github.com/opencog/cogutil
* It uses exactly the same build procedure as this package. Be sure
  to `sudo make install` at the end.

###### guile
* Embedded scheme REPL; version 3.0 or newer required.
* https://www.gnu.org/software/guile/guile.html
* For Debian/Ubuntu,  `apt install guile-3.0-dev`

###### cxxtest
* Unit test framework.
* Required for running unit tests. Breaking unit tests is verboten!
* https://cxxtest.com/ | `apt install cxxtest`

### Optional Prerequisites

The following packages are optional. If they are not installed, some
optional parts of the AtomSpace will not be built.  The `cmake` command,
during the build, will be more precise as to which parts will not be built.

###### Cython
* C bindings for Python. (Cython version 0.23 or newer)
* Recommended, as many users enjoy using python.
* https://cython.org | `apt install cython`

###### Haskell
* Haskell bindings (experimental).
* Optional; almost no existing code makes use of Haskell.
* https://www.haskell.org/

###### OCaml
* OCaml bindings (experimental).
* Optional; almost no existing code makes use of OCaml.
* https://www.ocaml.org/ | `apt install ocaml ocaml-findlib`

###### Postgres
* Distributed, multi-client networked storage.
* Needed for "remembering" between shutdowns (and for distributed AtomSpace)
* Optional; The RocksDB backend is recommended. Use the cogserver to get a
  distributed atomspace.
* https://postgres.org | `apt install postgresql postgresql-client libpq-dev`

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
    make -j4 check
```
Libraries will be built into subdirectories within build, mirroring
the structure of the source directory root.


### Unit tests

To build and run the unit tests, from the `./build` directory enter
(after building opencog as above):
```
    make -j4 check
```
Most tests (just not the database tests) can be run in parallel:
```
    make -j4 check ARGS=-j4
```
The database tests *will* fail if run in parallel: they will step on
one-another, since they all set and clear the same database tables.

Specific subsets of the unit tests can be run:
```
    make test_atomese
    make test_atomspace
    make test_guile
    make test_join
    make test_python
    make test_query
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
will work with either Scheme (guile), Python, C++, OCaml or Haskell.

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

TODO - Notes - Open Projects
============================
* Porting to Android: multiple issues described in
  [cogroid/b-obstacles](https://github.com/cogroid/b-obstacles).
  See also [bug 2995](https://github.com/opencog/atomspace/issues/2995).

* Bug: Crashes on Arm v7a:
  [bug 2944](https://github.com/opencog/atomspace/issues/2944).

Interesting Reading
===================
* Seems that the AtomSpace is no longer alone in the hypergraph world!
  As of 2022, one can find a python library called
  [HyperNetX](https://hypernetx.readthedocs.io/en/latest/).
  Their documentation is even eerily similar to our own! Gee, how could
  that happen?

* Stop the presses! There's more! Extra! Extra! Read all about it!
  [NetworkX](https://networkx.org/) is a python package for analyzing
  complex networks.
