
Atoms
=====

This directory contains C++ implementations of some, but not all,
certain atom types. Except for the base set, all of these atoms are
"special": they are extensions of the base atoms, intended to improve
the performance of the system.  Almost all of these atom types
"memoize" or "cache" some precomputed values, that are then used
during run-time to speed performance, and avoid repeated, redundant
computations.

See the [AtomSpace REAMDE](../atomspace/README) for a quick definition
of what an Atom is.

Subdirectories
--------------
 * base -- The basic atoms: ProtoAtom, Atom, Node, Link, Value.

 * bind -- Atoms involved with pattern matching: BindLink, LambdaLink,
   SatisfactionLink, SatisfactionSetLink, GetLink.  These cache some
   complicated data structures that are leveraged during patten
   matching.

 * core -- those C++ atoms that do not depend on other C++ subsystems
   e.g. do not depend on scheme, python, the pattern matcher, etc.
   They implement assorted simple but commonly desirable semantics.

 * execute -- "black-box" executable/evaluatable links, e.g.
   ExecutionOutputLink, EvaluationLink, GroundedPredicateNode, etc.
   These interact with the scheme, python and haskell evaluators.

 * reduct -- inspired by comboreduct, these are "clearbox" links:
   PlusLink, TimesLink, etc.  Obsolecent, probably should be
   re-thought and re-worked.
