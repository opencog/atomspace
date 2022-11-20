
Pattern Matcher Examples
========================
The AtomSpace is a knowledgebase: a collection of tools for working
with (semantic) knowledge stored in a graph database. Querying the
database is a basic building block, on top of which much of the rest
is built.

The pattern matcher implements a graph query language (GQL) with a
number of advanced features not found in ordinary graph databases.
This directory contains examples of how to use these.

Perhaps the most novel aspect is that the queries themselves are also
graphs, and so that they too can be stored in the knowledgebase.  This
in turn enables a kind of recursive query processing.

By storing queries as graphs, one can build a rule engine, by writing
each rule as a query. The rule engine performs a "meta"-query,
selecting appropriate rules, so as to chain them together. The rule
engine in turn can serve as a foundation for inferencing, theorem-
proving, logical deduction, common-sense reasoning, and the like.
The axioms of natural deduction (of Hilbert systems) are themselves
stored as graphs.

By storing each query as a graph, one can do inverse queries. So, for
example, one can search for all queries that match a given "answer".
This reverses the ordinary concept of queries: here, one knows the
answer (it's some graph), and one is looking for questions that might
deliver that answer (the questions are just other graphs).

Another important feature is that many Atomese Atoms are "active", and
cause things to happen when executed. The pattern matcher is a natural
place to trigger these. Thus, Atoms can lie dormant until a query causes
them to run. This is particularly useful for interacting with external
systems, to process sensor input and perform control (for example, to
process input from robotic sensors or game-world inputs, and to then
control the action of the robot or game-world character.)

The simplest example of an "active" link is the GreaterThanLink. As a
knowledge-base, it is impossible to passively store all possible
greater-than relationships between integers: there's a countable
infinity of them. The GreaterThanLink, when triggered during a search,
can procedurally (algorithmically) evaluate, on the spot, the relative
order of two integers.  The GreaterThanLink serves as an example of
arbitrary computation that can be done "behind the scenes". However,
(and this is the important part!) the "meaning" of greater-than is
known a-priori. Thus, the reasoning engine can make logical deductions
about what GreaterThanLink would have done, if it had been called.
Thus, it is both declarative (asserting a fact) and active (returns an
actual answer, if you ask it).

More complex examples include the arithmetic links: PlusLink and
TimesLink, which can add an multiply numbers; the ValueOfLink, which can
obtain numeric values, the FormulaPredicateLink, which can evaluate
predicateds on inputs, and the GroundedPredicateNode, which can convert
external stimuli into Atomese predicates.

These "active" link types can be composed to describe processing
pipelines and systems that are both accessible to reasoning and learning
(because they encode "knowledge" of the pipeline) and at the same time,
these processing pipleines can actually "do" whatever it is they are
wired up to do.  One can imagine the Atoms as describing pipes or
plumbing, and the values flowing through them as water flowing through
the pipes. The AtomSpace holds the current configuration of the pipes;
the query subsystem is used to edit and change the configuration; the
values move through the configuration.


Preliminaries
-------------
Please go through the first four or six or ten demos in the
[atomspace](../atomspace) folder first. These will provide an
introduction to the various features and functions in the AtomSpace
itself. This will make the examples here much easier to understand.

Many of the examples assume that you are cutting and pasting from the
example to the guile prompt. You might find it easier to bulk-load
the example, `(load-from-path "some-example.scm")` and then bounce
around inside of it.


Basic Examples
--------------
The first four examples provide a basic introduction to basic
pattern matching.

* `anchor.scm`       -- Obtaining results incrementally.
* `satisfaction.scm` -- Determining satisfiability of a query.
* `glob.scm`         -- Matching multiple atoms at once.
* `choice.scm`       -- Using the ChoiceLink to explore alternatives.


Presence and Absence
--------------------
Sometimes, one wants a query to match, only if some other sub-query does
not.  To find all graphs that do *not* have a certain subgraph in them.
This requires testing for absence.

Normally, a query consists of a collection of "clauses", each of which
must be matched. Any variables appearing in the clauses must have
consistent groundings: the clauses are treated as an "inner join".
Absence is handled in a very similar fashion: absent clauses are those
which do not have a common grounding, which cannot be joined to the
other (required) clauses.

(Cute factoid for the mathematically-inclined: this implies that the
pattern matcher implements a form of "intuitionistic logic", or
"constructivist logic" under the covers. Perhaps not a surprise: these
are the logics of theorem-proving, in general.)

* `presence.scm`     -- Testing for presence of an Atom ("there-exists").
* `absent.scm`       -- Using the AbsentLink ("there-does-not-exist").
* `always.scm`       -- Testing if a clause always holds ("for-all").
* `recursive.scm`    -- Recursive pattern matching aka forward chaining.
* `value-of.scm`     -- Looking for high or low TruthValues.
* `dot-product.scm`  -- Numeric computations on query results.
* `query.scm`        -- Running queries in parallel.


Pattern Recognition
-------------------
It is sometimes useful to invert a query. One might want to find all
"questions" that provide a given "answer". This is accomplished with
the pattern-recognizer, or `DualLink`. Given a graph without variables
in it (a "ground term"), it can find all graphs that do have variables
in them, that would have matched up, if aligned properly.

This inverted search for ungrounded patterns is very useful for building
a rule engine. Given a rule-set, it allows one to figure out very
quickly which rules can be chained together. Crudely speaking, one
can think of the recognizer as being kind-of-like a RETE algorithm.

The DualLink is particularly useful for constructing stimulus-response
("SRAI") systems, such a chatbots, where an input utterance (the
"stimulus") needs to be matched to any one of dozens of different chatbot
responses, depending on the current chat topic and other knowledge.

* `recognizer.scm`    -- Implementing AIML with DualLink.


Types
-----
Types allow queries to be constrained so that only certain types of
graphs are matched.  For example, one might want to find all graphs that
have a ConceptNode in one location, and a PredicateNode in a different
one. In this case, one constrains the pattern variable to be of type
"ConceptNode" or of type "PredicateNode".

More generally, one may want to define new types that are combinations
of existing types: these are the type constructors. For example, one
might want to find all "functions" that take certain "inputs" and
produce certain "outputs", where the inputs and outputs are of a certain
type. This can be achieved by the "function type" or "arrow type": a
type constructor that builds function types.

Atomese implements a fairly complete type system.  It provides all of
the basic type constructors; the biggest missing piece is support for
dependent types.

* `define.scm`         -- DefineLinks give names to sub-patterns.
* `type-signature.scm` -- Using signatures and type constructors.


Virtual Links
-------------
Virtual links are links that require algorithmic support to make a
decision. The primary example is GreaterThanLink. It is impossible
to store all pairs of numbers such that the first is greater than the
second. However, it is quite easy to run an algorithm to make the same
determination.  This is a "virtual link": it acts as if any given
ordered pair of numbers was actually in the AtomSpace, even though it
isn't, really. The pair is only "virtually" there, existing briefly,
just long enough to perform the determination.

Any relation that might require an infinitely-large set to instantiate
can be implemented as a virtual link.  The pattern matcher knows how to
join these together.

More precisely: A collection of clauses to be (collectively) satisfied
can be thought of as a Cartesian product of those clauses. The "inner
join", the need to have variables have common groundings during
satisfaction means that not every possible element of the product can
appear -- the join discards or filters away most members of the
Cartesian product.

This is easy to do for ordinary graphs that "really" exist in the
knowledgebase; its a good bit harder when the graph has virtual links in
it.  There is a potential combinatorial explosion: the Cartesian product
can become huge, and one cannot afford to execute a virtual link on
every potential combination. The pattern matcher takes steps to minimize
that combinatorial explosion.

* `virtual.scm`         -- Using virtual links.


Interacting with External Systems
---------------------------------
Atomese can be used to perform scripting to control external systems
(e.g. robots) in response to external stimuli (e.g. sensory inputs).
The scripts themselves are just collections of atoms, held in the
AtomSpace, describing behaviors and processing pipelines. The "active"
atom types can actually process data and perform actions, according to
the current configuration. The missing link is getting data to and from
the AtomSpace; this is provided by the GroundedPredicateNode and the
GroundedSchemaNode, which interface to code written in python, scheme
and C/C++, and can be used to import sensor data into the AtomSpace,
as well as to control robots or game characters outside the AtomSpace.

* `gsn.scm`            -- When a match is found, call a callback.
* `gsn-truth.scm`      -- Altering TruthValues in a callback.
* `gpn.scm`            -- Callback decides: is there a match?
* `sequence.scm`       -- Using GPN's to execute a sequence of tasks.
* `condition.scm`      -- Actions taken can depend on preconditions.


State Machines
--------------
State machines are a popular control strategy for robots and other
systems, and are an alternative to behavior trees (which were
demonstrated in earlier examples).

The pattern matcher is powerful enough to write state machines without
any further ado.  The pattern matcher itself is implemented as a stack
machine. As a result, it is fairly easy to implement state machines
simply by writing down the transition graphs (transition functions)
for them.

* `fsm-basic.scm`     -- A Deterministic Finite State Machine (FSM).
* `fsm-full.scm`      -- A generic deterministic FSM constructor.
* `fsm-mealy.scm`     -- A generic Mealy machine constructor.
* `markov-chain.scm`  -- A Markov chain (probabilistic FSM) based on fsm-full.


Filtering and Mapping
---------------------
Given a set of Atoms, one might want to filter out only portions of that
set. This can be done with `FilterLink`.  Given a set of Atoms, one
might want to apply some transformation to that set. This can be done
with `MapLink`.  This somewhat resembles the filtering predicates and
mapping functions found in
[srfi-1](https://srfi.schemers.org/srfi-1/srfi-1.html). The `MapLink`
is particularly interesting: it can be though of as an `UnPutLink`, to
undo the effects of a `PutLink`, that is, to extract data.

These two links are kind-of deprecated. They are a historical
experiment, and they overlap some of the core pattern matcher
functionality, but limiting it to just a subspace of the AtomSpace.
There is an open issue to resolve this situation, by providing a
unified interface that allows sub-spaces of the AtomSpace to be
specified. The goal is to allow subspaces to be thought of as contexts
or as "general frames".

(The pattern matcher implicitly implements a certain kind of modal
logic, under the covers and invisibly to the user.  May as well come
out of the closet about that, and allow full-blown contexts and general
frames.  It will take some work to do this.)

In the meanwhile, some awkward examples of searching, querying,
filtering and mapping, done sideways.

* `filter.scm`         -- Filtering sets of atoms with PutLink.
* `map.scm`            -- Extracting and re-writing with MapLink.

Unfinished examples
-------------------
Some experiments that get complicated. And can't easily be done.

* `deduction-engine.scm`  -- a ProLog-like reasoning engine
