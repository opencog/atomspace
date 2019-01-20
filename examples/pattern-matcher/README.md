
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

By storing queries as graphs, one can build a rule-engine, by writing
each rule as a query. The rule-engine performs a "meta"-query,
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
cause things to heppen when executed. The pattern matcher is a natural
place to trigger these. Thus, Atoms can lie dormant until a query causes
them to run.

The simplest example of this is the GreaterThanLink. As a
knowledge-base, it is impossible to passively store all possible
greater-than relationships between integers: there's a countable
infinity of them. The GreaterThanLink, when triggered during a search,
can proceedurally (algorithmically) evaluate, on the spot, the relative
order of two integers.  The GreaterThanLink serves as an example of
arbitrary computation that can be done "behind the scenes". However,
(and this is the important part!) the "meaning" of greater-than is
known a-priori. Thus, the reasoning engine can make logical deductions
about what GreaterThanLink would have done, if it had been called.
Thus, it is both declarative (asserting a fact) and active (returns an
actual answer, if you ask it).

Preliminaries
-------------
Please go through the first four or six or ten demos in the
[atomspace](../atomspace) folder first. These will provide an
introduction to the various features and functions in the AtomSpace
itself. This will make the examples here much easier to understand.

Basic Examples
--------------
The first four examples provide a basic introduction to basic
pattern matching.

* `simple.scm`       -- A basic introduction to pattern matching.
* `satisfcation.scm` -- Determining satisfiability of a query.
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

* `presence.scm`     -- Testing for the presence of an Atom.
* `absent.scm`       -- Using the AbsentLink.

Pattern Recognition
-------------------
It is sometimes useful to invert a query. One might want to find all
"questions" that provide a given "answer". This is accomplished with
the pattern-recognizer, or `DualLink`. Given a graph without variables
in it (a "ground term"), it can find all graphs that do have variables
in them, that would have matched up, if aligned properly.

This inverted search for ungrounded patterns is very useful for building
a rule-engine. Given a rule-set, it allows one to figure out very
quickly which rules can be chained together. (Crudely speaking, one
can think of the recognizer as being kind-of-like a RETE algorithm).

* `recognizer.scm`    -- Implementing AIML with DualLink.

Types
-----
Types allow queries to be constrainted so that only certain types of
graphs are matched.  For example, one might want to find all graphs that
have a ConceptNode in one location, and a PredicateNode in a different
one. In this case, one constrains the pattern variable to be of type
"ConceptNode" or of type "PredicateNode".

More generally, one may want to define new types that are combinations
of existing types: these are the type constructors. For example, one
might want to find all "functions" that take certain "inputs" and
produce certain "outputs", where the inputs and outputs are of a certain
type. This can be acheived by the "function type" or "arrow type": a
type constructor that builds function types.

Atomese implements a fairly complete type system.  It provides all of
the basic type constructors; the biggest missing piece is support for
dependent types.

* `define.scm`         -- DefineLinks give names to sub-patterns.
* `type-signature.scm` -- Using signatures and type constructors.

Triggering Side-Effects
-----------------------
The pattern matcher can be used to trigger side-effects, when a pattern
is matched.  This includes the execution of arbitrary code, both as
"black-box" code, as well as "clear-box" Atomese.

* `gsn.scm`            -- When a match is found, call a callback.
* `gpn.scm`            -- Callback decides: is there a match?
* `sequence.scm`       -- Using GPN's to execute a sequence of tasks.
* `condition.scm`      -- Actions taken can depend on preconditions.

State Machines
--------------
The pattern matcher is powerful enough to write state machines without
any further ado.  The pattern matcher itself is implemented as a stack
machine. As a result, it is fairly easy to implement state machines
simply by writing down the transition graphs (transition functions)
for them.

* `fsm-basic.scm`     -- A Deterministic Finite State Machine (FSM).
* `fsm-full.scm`      -- A generic deterministic FSM constructor.
* `fsm-mealy.scm`     -- A generic Mealy machine constructor.
* `markov-chain.scm`  -- A Markov chain (probabilistic FSM) based on fsm-full.

Examples that demonstrate some of the inner workings of the pattern
matcher.

* virtual.scm: Using virtual links, and the resulting combinatorial
     explosion.

Unfinished examples
-------------------
Some experiments that get complicated. And can't easily be done.

* `deduction-engine.scm`  -- a ProLog-like reasoning engine
