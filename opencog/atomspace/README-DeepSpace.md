Multiple AtomSpace Design
=========================

(See also the base [README](README.md) for a simpler, more basic
discussion of multiple AtomSpace design requirements.)

It has become clear that there is a need to be able to store and access
long chains of changesets to the AtomSpace. Snapshots of the AtomSpace
at a given point in time. It is necessary for software to be able to 
access all of these snapshots "at the same time", and to do soe with
minimal (zero) performance penalty.  These chains can be long: current
software requires a stack of at least 3000 changsets, one on top
another, and likely much more. There is not just one linear chain; there
is typically a branching tree of changes, although it seems the tree
will be much deeper than it is wide.

The primary user of this ability is the learning subsystem, where the
AtomSpace contains millions of Atoms, and thousands of these are
altered: usually, the Values on Atoms are altered, but also sometimes
new Atoms are added, and some Atoms are deleted.

A "change-set" is meant to convey the same idea as in git: a single,
coherent set of changes to many Atoms, rather than some piecemeal,
uncoordinated changes to individual Atoms.
