Pattern Matcher Examples
------------------------

See `../guile/README` for an intro on running the guile shell needed
to run the demos here.

The first set of demos provide a basic introduction to various
main features.

* simple.scm: A basic introduction to pattern matching.
* gsn.scm: Calling arbitrary functions upon a match.
* gpn.scm: Calling arbitrary functions to decide a match.
* sequence.scm: Using GPN's to execute a sequence of tasks.
* condition.scm: combining GPN's and GSN's to make an action
    taken depend on a precondition.
* absent.scm: Use the AssignLink to set state; use AbsentLink to check
    for the absence of atoms in the AtomSpace.

Some simpler applications showing some things one can do, and how to do
them.

* fsm-basic.scm: A simple Deterministic Finite State Machine demo.
* fsm-full.scm: A generic deterministic finite-state-machine constructor
* fsm-mealy.scm: A generic Mealy machine constructor
* markov-chain.scm: A Markov chain (probabilistic FSM) based on fsm-full.
