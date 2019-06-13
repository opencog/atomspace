
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

			; This nested Satsifaction should be treated as
			; an evaluatable clause by the pattern matcher,
			; and should be evaluated on demand.
			(Satisfaction (Variable "y")
				(And
					(Inheritance (Variable "x") (Variable "y"))
					(Not (Equal (Variable "y") (Concept "mammal")))
				))
		))
)
