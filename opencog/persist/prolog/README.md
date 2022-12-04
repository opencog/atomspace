
Prolog/Datalog encoding/decoding
--------------------------------
A subset of Prolog is easily encoded as Atomese; a subset of Atomese
is easily encoded as Prolog. The code here implements this encoding
and decoding. In one direction, given prolog text, it will import it,
and return Atomese Atoms. Conversely, given appropriate Atomese, it
can be printed as prolog.

The world "datalog" is used, because this is mostly just the declarative
part of prolog, and not the chaining and evaluation part.

The code works; see the
[Prolog/Datalog example](../../../examples/foreign/prolog-datalog.scm)
for a working demo.

This is a *proof of concept*. It can be, and should be, expanded into a
broader connectivity layer, including translation of chaining and
inference (*e.g.* by mapping to the Atomese `QueryLink`).

Mapping
-------
The mapping is very easy and obvious. The Prolog "fact"
```
    :- siblings(jack, jill).
```
becomes the (rather more verbose) Atomese
```
    (Evaluation
        (Predicate "siblings")
        (List
            (Concept "jack")
            (Concept "jill")))
```
The mapping for Horn clauses is this:
```
    child(X,Y) :- parent(Y,X).
```
becomes
```
    (Implication
        (Evaluation
            (Predicate "parent")
            (List
                (Variable "Y")
                (Variable "X")))
        (Evaluation
            (Predicate "child")
            (List
                (Variable "X")
                (Variable "Y"))))
```
Note that the order is reversed: `ImplicationLink`s encode `P->Q` as
opposed to prologs `Q :- P` turnstile symbol.

Comma-separated fragments are wrapped with an `AndLink`.
