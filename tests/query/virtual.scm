
(use-modules (opencog) (opencog exec))

(Inheritance (Concept "mouse") (Concept "animal"))
(Inheritance (Concept "mouse") (Concept "mammal"))

(define get-mouse
	(GetLink (Variable "x")
		(And
			(Inheritance (Variable "x") (Concept "animal"))

			; This nested Satsifaction should be treated as
			; an evaluatable clause by the pattern matcher,
			; and should be evaluated on demand.
			(Satisfaction (Variable "y")
				(And
					(Inheritance (Variable "x") (Variable "y"))
					(Equal (Variable "y") (Concept "mammal"))
				))
		))
)

(Inheritance (Concept "snail") (Concept "animal"))
(Inheritance (Concept "snail") (Concept "gastropod"))

(define get-snail
	(GetLink (Variable "x")
		(And
			(Inheritance (Variable "x") (Concept "animal"))

			; Find some inheritance relation that is not already known.
			(Satisfaction (Variable "y")
				(And
					(Inheritance (Variable "x") (Variable "y"))
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
			(Inheritance (Variable "x") (Concept "animal"))
			(Inheritance (Variable "y") (Concept "animal"))
			(Not (Equal (Variable "x") (Variable "y")))

			; Satisfiable, because they are both animals.
			(Satisfaction (Variable "z")
				(And
					(Inheritance (Variable "x") (Variable "z"))
					(Inheritance (Variable "y") (Variable "z"))
					(Inheritance (Variable "x") (Concept "mammal"))
				))
		))
)
