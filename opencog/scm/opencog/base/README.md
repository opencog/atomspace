
opencog base
------------
Assorted handy minor utilities for working with Atomese at the scheme
REPL shell. In general programmers are *discouraged* from writing code
in scheme, and *encouraged* to write code in straight-up Atomese!
Write your algos in Atomese, not scheme!

But this is easier said than done, so some minor utilities follow.

* utilities.scm --  Print summary reports of the AtomSpace; get
                    collections of Atoms attached to a given Atom; 
                    remove collections of Atoms.

* atom-cache.scm -- A scheme cache that implements an association list
                    between Atoms and arbitrary scheme code.
