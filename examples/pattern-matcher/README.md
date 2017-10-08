Pattern Matcher Examples
------------------------

See `../guile/README` for an intro on running the guile shell needed
to run the demos here.

The first three examples provide a basic introduction to basic
pattern matching.

* simple.scm: A basic introduction to pattern matching.
* satisfcation.scm: An example of using the SatisfactionLink
* glob.scm: Matching multiple atoms at once.
* choice.scm: Using the ChoiceLink to explore alternatives.

The pattern matcher can be used to trigger side-effects, when a pattern
is matched.  This includes the execution of arbitrary code, both as
"black-box" code, as well as "clear-box" Atomese.

* gsn.scm: Calling arbitrary functions upon a match.
* gpn.scm: Calling arbitrary functions to decide a match.
* sequence.scm: Using GPN's to execute a sequence of tasks.
* condition.scm: Combining GPN's and GSN's to make an action taken
    depend on a precondition.

The pattern matcher is a kind of graph query-engine; it is looking for
the presence (or absence) of certain graphs in the atomspace. This can
be understood to be a form of "intuitionistic logic", or
"constructivist logic".

* presence.scm: various ways of testing for the presence of an atom in
    the AtomSpace.
* absent.scm: Use the StateLink to set state; use AbsentLink to check
    for the absence of atoms in the AtomSpace.

The above examples should make clear that the pattern matcher implements
a kind-of programming langauge, refered to as "atomese". Like any good
programming language, it steals ideas.  One of these is the idea of
defining things.  Another is a fairly complete type system, providing
basic type constructors (although it currently stops short of providing
dependent types).

* define.scm: using DefineLinks to create patterns out of parts.
* type-signature.scm: using signatures and type constructors to refine
    the search.

The pattern matcher can be used to implement a rule engine.  In order
for rule engines to run efficiently, they need to implement the Rete
algorithm, or something similar.  The pattern-recognizer, or DualLink,
does this: it is used to find the rules that can be applied to a given
graph.

* recognizer.scm: using DualLink to implement an AIML-like system.

Some simpler applications showing some things one can do, and how to do
them.

* fsm-basic.scm: A simple Deterministic Finite State Machine demo.
* fsm-full.scm: A generic deterministic finite-state-machine constructor.
* fsm-mealy.scm: A generic Mealy machine constructor.
* markov-chain.scm: A Markov chain (probabilistic FSM) based on fsm-full.

Examples that demonstrate some of the inner workings of the pattern
matcher.

* virtual.scm: Using virtual links, and the resulting combinatorial
     explosion.

Unfinished examples:

* deduction-engine.scm: How to create a ProLog-like reasoning engine in
      Atomese.
