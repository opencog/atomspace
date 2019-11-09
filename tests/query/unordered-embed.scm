(use-modules (opencog) (opencog exec))

(List (Concept "A") (Set (Predicate "P") (Predicate "Q")))

(define embedded-set
	(Bind
		(Present (List (Variable "$C") (Set (Variable "$X") (Variable "$Y"))))
		(Implication (Variable "$X") (Variable "$Y"))))

; The expected answer from above.
(define expect-embedded-set
	(Set
		(Implication (Predicate "P") (Predicate "Q"))
		(Implication (Predicate "Q") (Predicate "P"))))
