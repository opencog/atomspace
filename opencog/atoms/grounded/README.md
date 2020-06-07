
Execution and Evaluation Support
================================

The code here implements execution support for GroundedPredicate and
GroundedSchema Nodes.

The execution of GroundedNodes requires that the argument atoms
be placed into the AtomSpace, prior to execution. This is for two
reasons:
  -- Both python and guile rely on the AtomSpace to find atoms.
     If the argument to a scheme/python function is not in the
     AtomSpace, bad things (crashes, exceptions) will happen.
  -- Black-box functions potentially need to examine the TV on the
     atom.  It is impossible to get an accurate TV value for an
     atom, unless that atom has been fished out of the AtomSpace.

Performance
-----------
Some reasons why grounded node execution/evaluation is slow, and ways
to speed that up.

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

Side effects
------------
The current design assumes that Grounded nodes can have side effects,
and thus, need to be run every time that they are encountered.  This
is a poor choice for good performance; a side-effect-free node would
be useful, and a monad when the side effects are needed.
