
Execution and Evaluation Support
================================

The code here implements execution support for GroundedPredicate and
GroundedSchema Nodes.

Design Notes
------------
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
Some ideas for improving execution speed.

* Fuction names are currently stored as strings. These could be
  decoded into SCM or PyObject just once, and cached. To do this,
  we would need to have a GroundedSCMNode and a GroundedPythonNode
  to hold these things.


Side effects
------------
The current design assumes that Grounded nodes can have side effects,
and thus, need to be run every time that they are encountered.  This
is a poor choice for good performance; a side-effect-free node would
be useful, and a monad when the side effects are needed.

TODO
----
There should be a DLPython.cc just like DLScheme.cc that will
dynamically load the needed Python libraries. This could/should
avoid assorted linking problems down-stream with Boost libraries,
and other linking hassles, whenever Python is enabled. Might be a
good to re-write Python to just not use Boost in the first place.
