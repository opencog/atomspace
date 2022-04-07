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

TODO
----
* Support atom deletion.
* Make the `AtomSpace::in_environ()` call, and related calls, run fast,
  instead of being a recursive walk on the C stack.  In general, lookups
  should be O(1) in stack depth, and not O(N).
