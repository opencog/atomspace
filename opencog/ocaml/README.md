
OCaml wrappers for the AtomSpace
================================

Unfinished prototype version 0.0.2

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

Prerequisites
-------------
Need to install the development tools:
```
apt install ocaml ocaml-findlib
```

Usage
-----

The ocaml interpreter can be started several different ways.
One way is to say, at the `bash` prompt:
```
$ rlwrap ocaml
#directory "/usr/local/lib/opencog/ocaml/" ;;
#use "atomese.ml" ;;
```
Note: the hash mark `#` must be typed in! The `#directory` and `#use`
are top-level directives given to the OCaml interpreter. Note that the
lines end with a double-semicolon.

A different way to start the beast is to give the directory to the
interpreter via the `-I` option:
```
$ rlwrap ocaml -I "/usr/local/lib/opencog/ocaml/"
#use "atomese.ml" ;;
```

Here are some basic things you can do, once you have the prompt:
```
concept "foo" ;;
cog_print_atomspace ()  ;;
let f = concept "foo" ;;
let b = concept "bar" ;;
let l = list_link [ f; b] ;;
cog_print_atomspace ()  ;;
let e = evaluation [ predicate "likely"; l] ;;
atom_sexpr e ;;
```

See the `examples/ocaml` directory for more examples.

Ideas
-----
* Use a 'META' file, so that everything can be loaded with `#require`.
* Doing
```
   sudo ocamlfind install atomspace dllcamlatoms.so atomspace.cma \
       atomspace.cmi atoms.cmi storage.cmi META
```
  installs stuff into `/usr/local/lib/ocaml/4.11.1/stublibs` and
  `ocamlfind list` will list the module. The module can be opened;
  but after opening, the shared library isn't loaded. Why ???

Debugging Hints
---------------
If things don't work, try this:
```
$ LD_LIBRARY_PATH=/usr/local/lib/opencog/ocaml/ rlwrap ocaml
```

Examine what's what:
```
$ ocamlobjinfo opencog/ocaml/atomspace.cma
```
