Unified Rule Engine examples
============================

Overview
--------

Collection of examples using the unified rule engine.

Usage
-----

First, you need to be able to run the examples in examples/guile and
examples/pattern-matcher. Have a look at examples/guile/README.md and
then examples/pattern-matcher/simple.scm.

The first set of demos provide a basic introduction to various main
features.

* crisp.scm: Example of a simple URE based crisp system.
* crisp-deduction.scm: Like crisp.scm but contains only the deduction rule

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
(PredicateNode "A" (stv 1 0.9))
```
means "A" is true.
```
(PredicateNode "B" (stv 0 0))
```
means "B" is unknown

Given the knowledge
```
(define A (PredicateNode "A" (stv 1 0.9)))
(define B (PredicateNode "B" (stv 0 0)))
(define AB (ImplicationLink A B (stv 1 0.9)))
```
The inference system should be able to produce
```
(PredicateNode "B" (stv 1 1))
```
by application of the crisp modus ponens rule.
