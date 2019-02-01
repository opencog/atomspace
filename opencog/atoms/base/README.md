
Atoms
=====
Atoms are well-documented on the wiki; see
[Category:Atom Types](http://wiki.opencog.org/w/Category:Atom_Types)
and [Atom](http://wiki.opencog.org/w/Atom)
and [Node](http://wiki.opencog.org/w/Node)
and [Link](http://wiki.opencog.org/w/Link).

ClassServer
===========
The ClassServer provides a factory for creating Atoms of different
types, given only thier type.

Over-use of Factory
--------------------
At this time, the factory is over-used: it gets called, even when there
is no need for it; this hurts performance. The problem is explained
below.

* A critical performance path is moving atoms from the API (python,
  scheme, Haskell, etc.) to the AtomSpace as fast as posssible. When a
  user creates a new atom (in scheme, python, etc.) they are actually
  just specifying the atom type, and either a string atom-name (for
  Nodes), or a sequence of existing atoms (for Links). The goal is to
  move the atom-type plus string/sequence to the AtomSpace as fast as
  possible.

* Once the atom-type+string/sequence gets to the AtomSpace, the factory
  should be used to create the "real" atom.  The "real" atom is an
  instance of the C++ class for that atom type.  There should be only
  one single, globally-unique copy of an Atom (ignoring multiple
  atomspaces, for now).  Thus, the factory should be run "only once",
  when an atom is inserted into the atomspace.

* The performance problem is that some (many?) atom types have
  complicated constructors that take a lot of CPU time to run. Thus,
  its a bad idea to run these constructors if the full C++ class is
  not actually needed. -- It might not be needed if one is merely
  trying to move the atom-type+string/sequence from one place to
  another.

* Thus, we need the concept of a "seedling" atom, which has nothing in
  it except for the atom-type, the name-string/handle-sequence, and
  the collection of Values.  This seedling avoids the overhead of the
  factory, and can provide the needed fast-path from the API languages
  (python, scheme, etc.) to the AtomSpace.

* Some (many?) of the `createNode` and `createLink` calls should be
  replaced by `createSeelingNode` and `createSeedlingLink`.
