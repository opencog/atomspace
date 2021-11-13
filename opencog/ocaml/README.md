
OCaml wrappers for the AtomSpace
================================

Unfinished prototype version 0.0.1

I had high hopes that the AtomSpace type system could be smoothly and
easily mapped to the OCaml type system. This does not appear to be
feasible at this point in time, for the following reasons:

* The OCaml type system is static; the AtomSpace type system is dynamic.
  That is, new types can be added to the AtomSpace at any time. It would
  take some effort to work around this restriction.

* The OCaml list insists that all list elements have the same type.
  By contrast, AtomSpace Links are lists of Atoms, which are
  polymorphic. It would take some effort to work around this restriction.

* OCaml does not provide any mechanisms for upcasting or downcasting
  types. This makes it difficult to cast an Atomese ConceptNode to an
  Atomese Node. Likewise, given a Node, it is hard to determine that
  it's type is actually a ConceptNode. It would take some effort to
  work around this restriction.

So although one could create a mapping of Atomese Atom Types to OCaml
types, and back, this would involve a fair amount of work to get it all
to behave 'naturally' in an OCaml environment.  Thus, it seems easiest,
at this point, to just treat Atoms as opaque types, and provide access
methods to them.  Thus, the OCaml binding here is much the same as the
Guile Scheme binding, which also treats Atoms as opaque objects.
