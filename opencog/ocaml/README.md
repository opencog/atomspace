
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

Prerequisites
-------------
Need to install the development tools:
```
apt install ocaml ocaml-findlib
```

Usage
-----

Semi-functional:
```
$ rlwrap ocaml -I /usr/local/lib/opencog/ocaml/
# #load "atomese.cma" ;;
# open Atomese ;;
# prtspace ()  ;;
# external concept : string -> atom = "new_ConceptNode" ;;
# concept "foo" ;;
# prtspace ()  ;;
```

Development
-----------
If you change `atomese.ml`, then you MUST run, by hand
```
ocamlc -i atomese.ml > atomese.mli
```
Yes, this should be automated in the CMakefile

Ideas
-----
* Use `#directory` instead of `-I`
* Use a 'META' file, so that everything can be loaded with `#require`.

Debugging Hints
---------------
If things don't work, try this:
```
$ LD_LIBRARY_PATH=/usr/local/lib/opencog/ocaml/ rlwrap ocaml
```

Examine what's what:
```
$ ocamlobjinfo opencog/ocaml/atomese.cma
```

open Atomese ;; fails unless I copy over atomese.cmi
