Example for understanding forward chaining.
Author: Mandeep Singh Bhatia
Date: 17 July 2015

This example comes from wikipedia article example on forward chaining.
https://en.wikipedia.org/wiki/Forward_chaining In this example we have
a black box for which we know there is something in it, and it makes
croaking sounds and eats flies.

All scheme code below is present in `frog.scm` and can be readily
loaded. Then you only need to run the pattern matcher and forward
chainer commands, see `cog-bind` and `cog-fc` respectively.

The objective now is to find the color of the thing in black box.  we
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

We define the first rule:
```scheme
(define rule1
  (BindLink
    (VariableList
      (VariableNode "$x")
    )
    (AndLink
      (InheritanceLink
        (VariableNode "$x")
        (ConceptNode "croaks")
      )
      (EvaluationLink
        (PredicateNode "eats")
        (ListLink
          (VariableNode "$x")
          (ConceptNode "flies")
        )
      )
    )
    (InheritanceLink
      (VariableNode "$x")
      (ConceptNode "frog")
    )
  )
)
```

The second rule is defined by:
```scheme
(define rule2
  (BindLink
    (VariableList
      (VariableNode "$x")
    )
    (InheritanceLink
      (VariableNode "$x")
      (ConceptNode "frog")
    )
    (InheritanceLink
      (VariableNode "$x")
      (ConceptNode "green")
    )
  )
)
```

Now if we create the known information that thing fritz coaks and eats flies
```scheme
(InheritanceLink
  (ConceptNode "fritz")
  (ConceptNode "croaks")
)
(EvaluationLink
  (PredicateNode "eats")
  (ListLink
    (ConceptNode "fritz")
    (ConceptNode "flies")
  )
)
```

Then running `(cog-bind rule1)` gives us the fact that fritz is a
frog, after that running `(cog-bind rule2)` gives us the result that
fritz is green.

If we want to do this by using forward chaining (which helps when
there exist large number of chain iterations to assert.) we first need
to define the rule-based system used to build the forward inference

Then we set the required information for forward chaining
```scheme
;-------------------------------------------
(define wiki (ConceptNode "wikipedia-rbs"))

(define rule1-name (DefinedSchemaNode "rule1"))
(DefineLink rule1-name rule1)

(define rule2-name (DefinedSchemaNode "rule2"))
(DefineLink rule2-name rule2)

(InheritanceLink  ; Defining a rule base
  wiki
  (ConceptNode "URE") ;; Special concept indicating the top rule-base
)

(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   wiki
   (NumberNode "100")
)

(MemberLink (stv 0.9 1)
  rule1-name
  wiki
)

(MemberLink (stv 0.5 1)
  rule2-name
  wiki
)
```

We need to define one of the above known information as a source
```scheme
(define source
  (InheritanceLink
    (ConceptNode "fritz")
    (ConceptNode "croaks")
  )
)
```

As well as its variable declaration. Since the source has no variable
the variable declaration is let undefined. It may always let
undefined, but if possible it is strongly recommanded to be defined as
it may speed up the URE
```scheme
(define vardecl
  (cog-undefined-handle)
)
```

Finally the focus set has to be defined. To set the focus over the
whole atomspace, either an undefined focus set, or an empty SetLink
may do, as below
```scheme
(define focus-set
  (cog-undefined-handle)
)
```

Then we run the forward chainer by
```scheme
(cog-fc wiki source vardecl focus-set)
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
(cog-bc wiki target vardel focus-set)
```
