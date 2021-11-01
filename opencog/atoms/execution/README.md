
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
The design has gotten complex due to multiple reasons.  But first,
here is why execution is complex:

* Execution must be done recursively, starting at the leafs.
  That is, most (but not all) functions require the function
  arguments to be in their final reduced 'value' form, before
  the function can be executed.  That is, execution does NOT
  commute with substitution.

* Substitution has to be done recursively, but with care: Not all
  variables are free; not all variables are bound. Thus, for example,
  the PutLink has two parts: all variables in the body of the PutLink
  are bound, but all variables in the value-list are free.  Thus,
  if there is variable substitution outside of PutLink, only the
  free variables can be substituted!

* The beta reduction of PutLink does commute with execution.
  That is, execution can be performed either before or after
  beta reduction. It is easier to handle execution of e.g.
  DeleteLink's, if execution is done before beta reduction.

* The execution of the DeleteLink cannot be done by itself, i.e.
  by it's own execute() method. This is because fully grounded
  (fully closed) DeleteLinks are forbidden, and cannot be inserted
  into the AtomSpace: thus, deletion needs to happen outside of
  its own execute() method.  This could be avoided if the AtomSpace
  told the atom when it was being inserted, but, right now, the
  AtomSpace does not send an "insert" message to the atoms being
  inserted.

* Execution is (almost always?) done in a "lazy" or delayed fashion.

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
