
Execution and Evaluation Support
================================

The code here implements a very crude form of execution support for
the AtomSpace. Its crude and painfully slow and inefficient.
Unfortunately, it is no longer simple, straight-forward.

Vocabulary
----------
Some vobabulary:

* "Substitution" means the substitution of values for thier place holders.
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
  -- Black-box functions, such as ExecutionOutputLink, which require
     scheme or python to be called.

* "Evaluation" means evaluating a function that returns a truth value as
  it's return value. There are black-box and clear-box variants of
  these.

The distinction between execution and evaluation is artificial, here:
at some abstract level, its the same thing; we just use the word
'evaluation' only when talking about functions that return TV's, and
'execution' when talking about functions that return atoms.


Design complexity
-----------------
The design has gotten complex due to mutiple reasons.  But first,
Here is why execution is complex:

* Execution must be done recursively, starting at the leafs.
  That is, must (but not all) functions require the function
  arguments to be in thier final reduced 'value' form, before
  the function can be executed.  That is, execution does NOT
  commute with substitution.

* The beta reduction of PutLink does commute with execution.
  That is, execution can be performed either before or after
  beta reduction. It is easier to handle execution of e.g.
  delete links if execution is done before beta reduction.

* The exeuction of black-box links requires that the argument atoms
  be placed into the atomspace, first. This is for two reasons:
  -- Both python and guile rely on the atomspace to find atoms.
     If the argument to a scheme/python function is not in the
     atomspace, bad things will probably happen.
  -- Black-box functions potentially need to examine the TV on the
     atom.  It is impossible to get an accurate TV value for an
     atom, unless that atom has been fished out of the atomspace.

* The execution of the Deletelink cannot be done by itself, i.e.
  by it's own execute() method. This is because fully grounded
  (fully closed) DeleteLinks are forbidden, and cannot be inserted
  into the atomspace: thus, deletion needs to happen outside of
  its own execute() method.  This could be avoided if the atomspace
  told the atom when it was being inserted, but, right now, the
  atomspace does not send an "insert" message to the atoms being
  inserted.


Garbage Collection
------------------
There's an implementation flaw that is hard to fix, right now, but
should be fixed.  When black-box links are encountered, thier
arguments are placed into the atomspace.  This is required for the
two reasons given above: TV values need to be fished out of the
atomspace, and also, python/scheme can't work with atoms that are not
in the atomspace.

What this means is that execution/evaluation leaves the atomspace
littered with partial results, and no effective way to clean them up
or garbage collect them.  Perhaps attention allocation can eventually do
this, but that is a very distant, abstract mechanism. Something more
immediate, to get the correct TV's, and have scheme/python not fail,
would be better.


Performance
-----------
Some reasons why black-box execution/evaluation is slow, and ways to
speed that up.

The code is slow because function names are currently stored as strings.
Thus, each time execution needs to be done, the string needs to be
decoded.  Next, after being decoded, either the guile, python or combo
interpreters need to be entered; this takes a fair amount of time.

The string decodes could be solved by decoding the string only once,
and caching that value (memoizing it).  The cached value would be the
SCM symbol, or the PyObject that the string name refers to.  Likewise,
the arguments: currently, just handles, could also be memoized (again,
as SCM's or PyObjects).

Where should these cached values be stored? Well, obviously, the best
place seems to be the Atom itself.  The memoized function should be
stored in the GSN/GPN atom.

The problem is that this makes the Atom fatter still (every Atom would
need to have an SCM in it, a PyObject in it, an Hs for Haskell...)
The alternative would be to use a map to store only the affected atoms.
The map should live in the atomspace, because deleted atoms would have
to be removed from the map, as well.  Thus, there would need to be a
calls such as this:

```
SCM AtomSpace::getSCM(Handle);
PyObject* AtomSpace::getPy(Handle);
```

Map look-ups are slower than direct-access in an atom...


Non-string values
-----------------
Some node types need to store one or more non-string values.  The
NumberNode here is a rough prototype for how that could be done. By
storing a double-precision floating point number (i.e. "caching" it)
it can potentially avoid string->double and double->string conversions
every time that it is accessed.  There are several caveats:

 * This works only if all possible Handles and AtomPtr's actually
   point to an instance of the NumberNode class, instead of an
   instance of a Node class, with the type set to NUMBER_NODE.

 * The above can happen only if the AtomSpace itself is careful to
   always work with instances of NumberNode, as it is the final
   arbiter of what a Handle points to.

 * The creation of a NumberNode is still "heavyweight": one must
   crate and initialize the Atom class (which takes time); one
   must also initialize the Node class, and to do that, one must
   convert the float pt. number to a string. The string is needed
   to allow the atom to be held in AtomSpace indexes.

 * The NumberNode must be inserted into the AtomSpace. This is a
   particularly time-consuming process, require multiple lookups and
   checks and validations, insertion into multiple indexes of various
   sorts. This far exceeds the cost of initializing the atom itself.

After the above is all done, the actual float pt. value in the
NumberNode can be accessed quickly enough ... but how often is that
really needed?  Was it really worth the additional complexity? Given
the other high costs shown above, does it result in any actual speedup?

For the above reasons, it is not at all clear that having a NumberNode
class is actually a good idea.  There could be a better way...


Side effects
------------
There is also another theoretical issue: the current design assumes
that ExecutionOutputLink can have side effects, and thus, it needs
to be run every time that it is encountered.  This is a poor choice
for good performance; we need to define a side-effect-free execution
link, and we need to define a monad when the side effects are needed.

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
