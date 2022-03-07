
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
types, given only their type (and, for Nodes, the string giving the node
name; for Links, the atom sequence).

The ClassServer also provides a simple constructor-time type-checking
and type-validation system. Many atoms can be thought of as "taking
inputs", while others "generate outputs", and the type-validation system
can check that these constraints are obeyed, at the time that the C++
class is first constructed.  There are also other type-checking and
type validation systems in the AtomSpace, this is just one of them.

At this time, the type validation system works closely with the
following types, declared in the
[`atom_types.script`](../atom_types/atom_types.script) file:

* `EVALUATABLE_LINK`, `BOOLEAN_LINK`, `NUMERIC_INPUT_LINK`,
  `NUMERIC_OUTPUT_LINK`, `TYPE_INPUT_LINK`, `TYPE_OUTPUT_LINK`

See the `atom_types.script` file for documentation (near lines 65-107)
and examples (in the lines that follow). These six link types cause
"validators" to be installed in the ClassServer; the ClassServer then
runs these validators, before calling the factory itself. This
validation helps avoid the need to have lots of repetitive checking
in the constructors for the various C++ atom classes; they also work
for atoms that do not have any C++ class behind them.

Weak Pointers
=============
Handles are reference-counted pointers to Atoms, guaranteeing that the
memory for an Atom is never released, as long as there is *some* Handle
pointing to it.  Links are vectors of Handles, and thus for a reference-
counted tree of all the Atoms underneath.

The Incoming Set points in the *opposite direction*, back up the tree.
One cannot use a regular "strong" (reference-counted) pointer for this,
as this would create a loop with a cycle, and thus could never decrement
itself back down to zero if there are no external references to the cycle.

The conventional wisdom here is to use weak pointers, as these will not
interfere with reference counting, while stil enabling safe memory access.
The code for the incoming set was originally written to use weak pointers.

However ... they are not really needed, and, instead, naked, bare pointers
can be used for the incoming set. This is because the incoming set is
always valid, when an Atom is in the AtomSpace... because incoming sets
are kept *only* when an Atom is in an AtomSpace, and are *never* kept when
an Atom is *not* in an AtomSpace!

The use of naked, bare backpointers in the incoming set allows for faster
dereferencing, avoiding the lock and the overhead of conveting weak pointers
into strong pointers. The `#define USE_BARE_BACKPOINTER 1` is set.
