
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
