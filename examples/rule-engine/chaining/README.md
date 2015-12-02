Example for understanding forward chaining.
Author: Mandeep Singh Bhatia
Date: 17 July 2015
This example comes from wikipedia article example on forward chaining.
https://en.wikipedia.org/wiki/Forward_chaining
In this example we have a black box for which we know there is something in it, and it makes croaking sounds and eats flies.
The objective now is to find the color of the thing in black box.
we have the following relations defined
```
1. If X croaks and X eats flies - Then X is a frog
2. If X is a frog - Then X is green
```
Let's say the thing in black box is named fritz and from above relations we need to deduce its color.

We define the first rule:
```
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
```
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
```
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

Then running `(cog-bind rule1)` gives us the fact that fritz is a frog, after that
running `(cog-bind rule2)` gives us the result that fritz is green.

If we want to do this by using forward chaining (which helps when there exist large number of chain iterations to assert.)
We need to define one of the above known information as a source
```
(define source
	(InheritanceLink
		(ConceptNode "fritz")
		(ConceptNode "croaks")
	)
)
```

Then we set the required information for forward chaining
```
;-------------------------------------------
(define wiki (ConceptNode "wikipedia-fc"))

(define rule1-name (Node "rule1"))
(DefineLink rule1-name rule1)

(define rule2-name (Node "rule2"))
(DefineLink rule2-name rule2)

(InheritanceLink  ; Defining a rule base
	(ConceptNode "wikipedia-fc")
	(ConceptNode "URE")
)

(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "wikipedia-fc")
   (NumberNode "100")
)

(MemberLink (stv 0.9 1)
	rule1-name
	(ConceptNode "wikipedia-fc")
)

(MemberLink (stv 0.5 1)
	rule2-name
	(ConceptNode "wikipedia-fc")
)
```

Then we run the forward chainer by
```
(cog-fc source wiki (SetLink))
```
to get the result that fritz is a frog and fritz is green.

Additionally, we can run the backward chainer to find out "What is green?" by
```
(cog-bc (InheritanceLink (VariableNode "$what") (ConceptNode "green")) wiki (SetLink))
```
