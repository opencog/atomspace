# Unified Rule Engine Frog Example

Example for understanding forward and backward chaining using the
Unified Rule Engine (URE).

## Overview

This example comes from the wikipedia article example on forward
chaining.  https://en.wikipedia.org/wiki/Forward_chaining In this
example we have a black box for which we know there is something in
it, and it makes croaking sounds and eats flies.

The objective is to find the color of the thing in the black box. We
have the following relations defined

```
1. If X croaks and X eats flies - Then X is a frog
2. If X is a frog - Then X is green
```

Let's say the thing in black box is named Fritz and from above
relations we need to deduce its color.

In the following sections will show different ways to solve this
problem. But before that some modules must be loaded, the query module
for using the pattern matcher, and the rule-engine module for using
URE.

```scheme
(use-modules (opencog query))
(use-modules (opencog rule-engine))
```

## Pattern Matcher

Then load the knowledge based containing the 2 relationships above,
plus the fact that Fritz coaks and eats flies
```scheme
(load "knowledge-base.scm")
```

Next, load the conditional instantiation rule base, that contains
conjunction introduction rule and conditional instantiation meta-rule
to solve the inference problem
```scheme
(load "rule-base.scm")
```

We apply the rules manually to understand what they do. The following
```scheme
(cog-bind fuzzy-conjunction-introduction-2ary-rule)
```
produces conjunctions including the fact that Fritz croaks and eats flies
```scheme
(SetLink
...
   (AndLink (stv 1 1)
      (EvaluationLink (stv 1 1)
         (PredicateNode "eats_flies")
         (ConceptNode "Fritz")
      )
      (EvaluationLink (stv 1 1)
         (PredicateNode "croaks")
         (ConceptNode "Fritz")
      )
   )
...
)
```

We then apply the conditional full instantiation meta rule. Since it
is a meta rule, that is it produces rules, we need to call `cog-bind`
twice, over the meta-rule, and then over the rules produced by the
meta-rule, thus the use of the scheme function `map`
```scheme
(map cog-bind (cog-outgoing-set (cog-bind conditional-full-instantiation-meta-rule)))
```
which gives us the fact that Fritz is a frog.

Running again
```scheme
(map cog-bind (cog-outgoing-set (cog-bind conditional-full-instantiation-meta-rule)))
```
finally gives us the result that Fritz is green.

## Pattern Matcher (using the frog rule base)

This is similar to the above but the implications are directly
represented as URE rules. This may not be the typical way to use the
URE but it shows a different perspective on how to use the URE to this
problem.

But let's first clear the atomspace to remove the solutions found in
the previous section
```scheme
(clear)
```

Then load the frog knowledge base
```scheme
(load "frog-rule-base.scm")
```

The user is obviously invited to compare `knowledge-base.scm` and
`frog-kb.scm` to understand how implications can be coded as rules.

Then we apply the rules
```scheme
(cog-bind if-croaks-and-eats-flies-then-frog-rule)
(cog-bind if-frog-then-green-rule)
```
to produce the answer.

Note that these rules do not assigne a truth value to their
output. This can be remedied by wrapping the rewrite term in a
ExecutionOutputLink with a grounded schema node that would assign the
truth value of the produced atom. But that is only optional, at least
for the pattern matcher and the forward chainer.

## Forward Chainer

Let's first clear the atomspace to remove the solutions found in the
previous section, and reload the knowledge and rule bases
```scheme
(clear)
(load "knowledge-base.scm")
(load "rule-base.scm")
```

To use the forward chainer we first need to define a source. Let's use
the fact that Fritz croaks. The fact that Fritz eats flies could have
been used too.
```scheme
(define source
  (Evaluation (stv 1.0 1.0)
     (Predicate "croaks")
     (Concept "Fritz"))
)
```

We need to define its variable declaration as well. Since the source
has no variable we can let it undefined by using the empty List, which
should not be confused with the empty VariableList which would define
an empty variable declaration. Though here it's irrelevant since the
source has no variable anyway
```scheme
(define vardecl (List))
```

Finally the focus set has to be defined. To set the focus to the whole
atomspace we define it as the empty set as below
```scheme
(define focus-set (Set))
```

We can now run the forward chainer
```scheme
(cog-fc ci-rbs source vardecl focus-set)
```
to get the result that fritz is a frog and fritz is green.

## Forward Chainer (using the frog rule base)

Let's clear the atomspace and load the frog rule base that encodes the
relationships as rules.
```scheme
(clear)
(load "knowledge-base.scm")
(load "frog-rule-base.scm")
```

Next, we re-define the source, its variable declaration and focus-set
as above
```scheme
(define source
  (Evaluation (stv 1.0 1.0)
     (Predicate "croaks")
     (Concept "Fritz")))
(define vardecl (List))
(define focus-set (Set))
```

We can now run the forward chainer
```scheme
(cog-fc frog-rb source vardecl focus-set)
```
to get the result that fritz is a frog and fritz is green.

## Backward Chainer

Let's clear the atomspace to remove the solutions and reload the
knowledge and rule bases
```scheme
(clear)
(load "knowledge-base.scm")
(load "rule-base.scm")
```

We can run the backward chainer to find out "What is green?" by
defining a target instead of a source
```scheme
(define target
  (InheritanceLink (VariableNode "$what") (ConceptNode "green"))
)
```

with the following variable declaration
```scheme
(define vardecl
  (TypedVariable (VariableNode "$what") (TypeNode "ConceptNode"))
)
```

and empty focus set (i.e. whole atomspace) a before
```scheme
(define focus-set (Set))
```

We can now call the backward chainer as follows
```scheme
(cog-bc ci-rbs target vardecl focus-set)
```
and get the answer that Fritz is green.

The alternate way using the frog rule base does not work with the
backward chainer because it requires the rewrite term to be a formula.

## Authors

Mandeep Singh Bhatia, Nil Geisweiller
Date: 17 July 2015, 07 April 2017
