
Execution and Evaluation Support
================================

The code here implements a crude form of execution support for the
AtomSpace. It is painfully slow and inefficient.  Unfortunately, it is
no longer simple or straight-forward.

Some future version of this should do two things:
* Move much of the execution/evaluation to C++ classes for each
  specific Atom type.
* Provide some kind of VM or byte-code to allow for fast, efficient
  execution/evaluation.

Vocabulary
----------
Some vocabulary:

* "Substitution" or "beta reduction" means the substitution of values
  for their place holders.
  There are several variants:
  -- The substitution of grounded values for variables (done by look-up
     in a map from variables to values)
  -- The substitution of a function by the result of executing the
     function.
  -- The beta reduction performed by PutLink

* "Execution" means executing a function that returns an atom as it's
  return value. There are several variants:
  -- Clear-box functions, such as PlusLink, which have an execute method
     that must be called.
  -- Black-box functions, such as GroundedSchemaNode, which require
     scheme or python to be called.

* "Evaluation" means evaluating a function that returns a truth value as
  it's return value. There are black-box and clear-box variants of
  these.

The distinction between execution and evaluation is a historical
artifact, dating back to when TruthValues had an elevated, special
meaning in the type hierarchy. This is now gone, and, for the most part,
execution and evaluation could be the same thing.

This distinction is slowly being erased, on an as-needed, as-possible
basis.  We are doing itt slowly because it is not always immediately
clear exactly how to do this correctly.

However: there is a distinction between evaluations that return crisp
true-false values, vs. other values. For performance reasons, we want to
always represent crisp true-false values with c++ booleans, so that the
code runs fast. Currently, this is done in an ad hoc manner, as needed.
A sharper architectural distinction would be nice.


Design complexity
-----------------
The design has gotten complex due to some confusion about when execution
should be done. In simplistic terms, there is confusion between "eager
execution" and "lazy execution".  Some functions need to execute their
arguments, before they can do what they do, while others must have
arguments applying themselves. Others must not, as that would ruin what
they do.

Substitution does not commute with execution. During substitution,
values need to be pasted into locations where variables stood. Of
course, bound variables and quoted variables cannot be substituted.

The handling of DeleteLink poses a challenge. If the result of
substitution into a DeleteLink results in a fully grounded (fully
closed) DeleteLink, the result (and anything that contains that result)
cannot be placed into the AtomSpace.


Performance
-----------
The execution/evaluation of clear-box links is not as good as it could
be.  Here's things that could be done:

1) If a clear-box link has no black boxes under it, and also no free
variables, and also does not depend on the TV, then the result of
execution/evaluation could be cached. i.e. multiple executions won't
change things.

2) In various cases, the Instatiator::execute() method can be called
twice: once to do the actual work, and a second time on the final
values, where it should return trivially.  This is hard to avoid, as
there are other call scenarios where this does not happen.

Demo
----
Example:
```
  (define two (NumberNode 2))
  (define plu (PlusLink two two))
  (cog-execute! plu)
```
Some other expressions:
```
  (cog-execute! (PlusLink (NumberNode 3) (NumberNode 2)))
  (cog-execute! (TimesLink (NumberNode 3) (NumberNode 2)))
  (cog-execute! (TimesLink (NumberNode 4)
     (PlusLink (NumberNode 5) (NumberNode 1))))
```
Inequalities:
```
  (define g (GreaterThanLink (NumberNode 3) (NumberNode 2)))
  (cog-evaluate! g)

  (cog-evaluate! (GreaterThanLink (NumberNode 3) (NumberNode 42)))

  (cog-evaluate! (GreaterThanLink (NumberNode 3)
     (PlusLink (NumberNode 6) (NumberNode 7))))

  (cog-evaluate! (NotLink (GreaterThanLink (NumberNode 3)
     (PlusLink (NumberNode 6) (NumberNode 7)))))
```
