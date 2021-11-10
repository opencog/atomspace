
(use-modules (opencog) (opencog exec))

(Inheritance (Concept "mouse") (Concept "animal"))
(Inheritance (Concept "mouse") (Concept "mammal"))

(define get-mouse
	(GetLink (Variable "x")
		(And
			(Present (Inheritance (Variable "x") (Concept "animal")))

			; This nested Satsifaction should be treated as
			; an evaluatable clause by the pattern matcher,
			; and should be evaluated on demand.
			(Satisfaction (Variable "y")
				(And
					(Present (Inheritance (Variable "x") (Variable "y")))
					(Equal (Variable "y") (Concept "mammal"))
				))
		))
)

; (cog-execute! get-mouse)

(Inheritance (Concept "snail") (Concept "animal"))
(Inheritance (Concept "snail") (Concept "gastropod"))

(define get-snail
	(GetLink (TypedVariable (Variable "x") (Type 'ConceptNode))
		(And
			(Present (Inheritance (Variable "x") (Concept "animal")))

			; Find some inheritance relation that is not already known.
			(Satisfaction (TypedVariable (Variable "y") (Type 'ConceptNode))
				(And
					(Present (Inheritance (Variable "x") (Variable "y")))
					(Not (Equal (Variable "y") (Concept "animal")))
					(Not (Equal (Variable "y") (Concept "mammal")))
					(Not (Equal (Variable "y") (Concept "cephalopod")))
				))
		))
)

(Inheritance (Concept "squid") (Concept "animal"))
(Inheritance (Concept "squid") (Concept "cephalopod"))

; Grounding this one requires the joining-together of two
; disconnected sub-patterns, that only the SatisfactionLink
; can perform. This should go down the same code as any
; other multi-component search.
(define get-mouse-pair
	(GetLink (VariableList
			(TypedVariable (Variable "x") (Type 'ConceptNode))
			(TypedVariable (Variable "y") (Type 'ConceptNode)))
		(And
			(Present (Inheritance (Variable "x") (Concept "animal")))
			(Present (Inheritance (Variable "x") (Concept "mammal")))
			(Present (Inheritance (Variable "y") (Concept "animal")))
			(Not (Equal (Variable "x") (Variable "y")))

			; Satisfiable, because there are InheritanceLinks
			; that both x and y inherit from (both are animals).
			(Satisfaction (Variable "z")
				(And
					(Present (Inheritance (Variable "x") (Variable "z")))
					(Present (Inheritance (Variable "y") (Variable "z")))
				))
		))
)

*unspecified*
