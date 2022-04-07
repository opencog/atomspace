Multiple AtomSpace Design
=========================

(See also the base [README](README.md) for a simpler, more basic
discussion of multiple AtomSpace design requirements.)

It has become clear that there is a need to be able to store and access
long chains of changesets to the AtomSpace. Such changesets occur during
learning and other kinds of inference. Storing them allows them to be
accessed at later times, which can useful in various ways, including the
ability to compare the contents of different chains (branches).

General requirements:
* It is necessary for software to be able to access all of these
  snapshots "at the same time", and to do so with minimal (zero)
  performance penalty.
* These chains can be long: current software requires a stack of at
  least 3000 changsets, one on top another, and likely much more.
* There is a need to store multiple branching chains. Current software
  suggests that this tree will usually be much deeper than it is wide.

The primary user of this ability is the learning subsystem, where the
AtomSpace contains millions of Atoms, and thousands of these are
altered: usually, the Values on Atoms are altered, but also sometimes
new Atoms are added, and some Atoms are deleted.

The URE might be able to use this ability, but currently, it appears
that it has invented its own subsystem for doing somethig similar.

A "change-set" is meant to convey the same idea as in git: a single,
coherent set of changes to many Atoms, rather than some piecemeal,
uncoordinated changes to individual Atoms.

Implementation
--------------
Atom deletion in an overaly is implemented by adding the atom to the
overlay, but then marking it as "absent". That way, the atom is still
there in the base space, while quieries for it in the cover space return
"no such atom".

Incoming set traversal
----------------------
The current design does NOT duplicate the incoming set of a covering
atom. This can lead to confusion for the unwary user.  Some examples are
in order.
```
(use-modules (opencog))

; Place Atom in the base-space.
(Concept "foo" (ctv 1 0 3))
(ListLink (Concept "foo") (Concept "bar"))

; Create a covering space
(define base-space (cog-atomspace))
(cog-atomspace-cow! #t base-space)

(define cover-space (cog-new-atomspace base-space))
(cog-set-atomspace! cover-space)
(cog-atomspace-cow! #t cover-space)

; Hide the atom "foo" in the base-space.
(Concept "foo" (ctv 1 0 42))

; Surprise! It has an empty incoming set!
(cog-incoming-set (Concept "foo"))

; Go back to the base space ... we see it is still there.
(cog-set-atomspace! base-space)
(cog-incoming-set (Concept "foo"))
```

Traversal is problematic, as well.
```
; Go back to the covering space
(cog-set-atomspace! cover-space)

; Surprise! There's a ListLink, but it contains the base-space
; version of (Concept "foo") and not the covering-space version,
; even though we are explicitly working in the covering space.
(cog-incoming-set (Concept "bar"))
```

These two surprises could be "fixed" in the code, but all of the
obvious fixes pay a hefty performance penalty. Thus, the current
implementation leaves it to the user to decide what to do.


TODO
----
* Make the `AtomSpace::in_environ()` call, and related calls, run fast,
  instead of being a recursive walk on the C stack.  In general, lookups
  should be O(1) in stack depth, and not O(N).
