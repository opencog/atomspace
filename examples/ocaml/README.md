
OCaml AtomSpace Examples
========================

This directory contains examples for how to use the OCaml language
bindings to work with Atoms and the AtomSpace.

Install
-------
Before the AtomSpace is compiled, you must first install the OCaml
development tools. On Debian/Ubuntu/Mint, say:

```
apt install ocaml ocaml-findlib
```
Then, as usual, `mkdir build; cmake ..; make` and so on.

Running
-------
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

A third option is to edit `~/.ocamlinit` and add thiese two lines to it:
```
#directory "/usr/local/lib/opencog/ocaml/" ;;
#use "atomese.ml" ;;
```
That way, Atomese is always accessible.

After that, each of the examples can be run individually, by loading
them as `#use "example.ml"`.

There are currently two examples:

* `basics.ml` -- The simplest base example of creating Atoms.
* `query.ml`  -- Running queries, as usual.

'As usual' here means that you are familiar with the far more extensive
scheme (guile) examples and AtomSpace API.

Comments on the Type System
---------------------------
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
