Unified Rule Engine examples
============================

Overview
--------

Here is a collection of examples (for now one) using the unified rule
engine.

Usage
-----

TODO: Something that looks like what is described in examples/guile or
examples/pattern-matcher. Unfortunately I wasn't able to run any so I
can't provide specific instructions yet.

Crisp deduction and modus ponens chainer
----------------------------------------

A simple crisp inference system with 2 rules, deduction and modus
ponens.

TVs here are used in a crisp way, that is

1. If an atom has TV.confidence < 0.5 then the atom is considered
unknown.

2. If an atom has TV.confidence >= 0.5, then TV.strength < 0.5 means
it is false, TV.strength >= 0.5 means it is true.

For instance
```
(ConceptNode "A" (stv 1 0.9))
```
means "A" is true.
```
(ConceptNode "B" (stv 0 0))
```
means "B" is unknown

Given the knowledge
```
(define A (ConceptNode "A" (stv 1 0.9)))
(define B (ConceptNode "B" (stv 0 0)))
(define AB (ImplicationLink A B (stv 1 0.9)))
```
The inference system should be able to produce
```
(ConceptNode "B" (stv 1 1))
```
by application of the crisp modus ponens rule.
