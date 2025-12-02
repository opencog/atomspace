Constraints
-----------

This directory implements a number of Atoms that encode axiomatic
relations that (also) constrain what can be placed in the AtomSpace.
That's a mouthful, and so is best explained by example.

Consider

   (Equal (Number 2) (Variable "X"))

This is an entirely valid relation, is is commonly used in the query
engine to determine if (Variable "X") has been grounded in an acceptable
manner. This is a (mathematical) relation (in the formal sense of
"relation" as used in "relational algebra".) It is evaluated at run
time, once a proposed value for X becomes available. This is done via
the `EqualLink::bevaluate()` method, which returns a crisp true/false.

Consider

   (Equal (Number 2) (Number 3))

This is insane and should not be allowed in the AtomSpace. When
insertion is attempted, an exception will be thrown.

This behavior is "experimental"; it can catch obvious abuses, but
clearly cannot enforce rigorous truth. Consider, for example:

  (Equal (Number 2) (BlackBoxThatSeemsToAlwaysReturnTwo))

Should this be allowed, or not? Just becuse it returned 2 the last
100 times does not mean it will return 2 the next time. There are
some simpler cases, like

  (Equal (Number 2) (Plus (Number 3) (Number 4)))

that can be evaluated, because the behavior of PlusLink is not in doubt.
But this is a simple case; anything more complicated seems to require
consulting some theorem provier or other oracle to find out for sure.

So, for now, this directory implements some common and commonly used
relational axioms that are actually needed by the pattern engine, and
by other processes. A gimlet eye is cast on the nature of axioms, and
thier relationship to the current ad hoc collection of relations and
functions. How this works out remains deeply mysterious, for now.

Quick Overview
--------------
A quick review:
 * `Present`: is this term present in the AtomSpace?
 * `Absent`: is this term not present? (For technical reasons, it
   is better ask absent? than not present? (It's easier to avoid
   assuming the law of excluded middle during pattern search.))
 * `Delete`: after being evaluated, the resulting Atom is deleted
   (removed, extracted) from the AtomSpace.

 * `Identical`: are these objets the same object? (syntactic equality)
 * `Equal`: are these objects the same, after being evaluated?
   (semantic equality)
 * `Exclusive`: are these objects all different?

 * `AlphaEqual`: are these lambda terms alpha-convertible to
   one-another?
 * `IsClosed`: does this term have any free variables, or not?
