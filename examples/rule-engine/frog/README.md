Example for understanding forward and backward chaining.
Authors: Mandeep Singh Bhatia, Nil Geisweiller
Date: 17 July 2015, 07 April 2017

This example comes from wikipedia article example on forward chaining.
https://en.wikipedia.org/wiki/Forward_chaining In this example we have
a black box for which we know there is something in it, and it makes
croaking sounds and eats flies.

All scheme code below is present in `frog.scm` and can be readily
loaded. Then you only need to run the pattern matcher, forward chainer
and backward chainer commands. See `cog-bind`, `cog-fc` and `cog-bc`
respectively. Otherwise follow right below.

The objective is to find the color of the thing in the black box. We
have the following relations defined

```
1. If X croaks and X eats flies - Then X is a frog
2. If X is a frog - Then X is green
```

Let's say the thing in black box is named fritz and from above
relations we need to deduce its color.

First load the query and the rule-engine modules
```scheme
(use-modules (opencog query))
(use-modules (opencog rule-engine))
```

We define the first implication:
```scheme
(ImplicationScope (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (And
      (Evaluation
         (Predicate "croaks")
         (Variable "$X"))
      (Evaluation
         (Predicate "eats_flies")
         (Variable "$X")))
   (Inheritance
      (Variable "$X")
      (Concept "Frog")))
```

The second implication:
```scheme
(ImplicationScope (stv 1.0 1.0)
   (TypedVariable
      (Variable "$X")
      (Type "ConceptNode"))
   (Inheritance
      (Variable "$X")
      (Concept "Frog"))
   (Inheritance
      (Variable "$X")
      (Concept "green")))
```

Now if we create the known information that fritz coaks and eats flies
```scheme
(Evaluation (stv 1.0 1.0)
   (Predicate "croaks")
   (Concept "Fritz"))
(Evaluation (stv 1.0 1.0)
   (Predicate "eats_flies")
   (Concept "Fritz"))
```

We load the condittional-instantiation rule base, that contains
conjunction introduction rule and conditional instantiation meta-rule
to solve the inference problem
```scheme
(load "conditional-instantiation-config.scm")
```

We can apply the rules and meta-rules manually as followed
```scheme
TODO
```
which gives us the fact that fritz is a frog, after that running
```scheme
TODO
```
gives us the result that fritz is green.

If we want to do this by using forward chaining (which helps when
there exist large number of chain iterations to assert.) we can call
the forward chainer on the source that Fritz croaks
```scheme
(define source
  (Evaluation (stv 1.0 1.0)
     (Predicate "croaks")
     (Concept "Fritz"))
)
```

We need to define its variable declaration as well. Though it has none
we can simply let it undefined by using an empty List. This should not
be confused with the empty VariableList which would defined an empty
variable declaration. Though here it's irrelevant since the source has
no variable anyway
```scheme
(define vardecl (List))
```

Finally the focus set has to be defined. To set the focus over the
whole atomspace we define an empty set as below
```scheme
(define focus-set (Set))
```

Then we can run the forward chainer
```scheme
(cog-fc ci-rbs source vardecl focus-set)
```
to get the result that fritz is a frog and fritz is green.

Additionally, we can run the backward chainer to find out "What is
green?" by defining a target instead of a source
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

and call the backward chainer as follows (re-using the undefined focus
set defined above)
```scheme
(cog-bc ci-rbs target vardecl focus-set)
```
