
(use-modules (opencog) (opencog exec))

;A workig 
; ----------------------
; Fancier version -- the dot product

; A pair of vectors
(Evaluation (Predicate "has legs") (Concept "dog") (CountTruthValue 1 0 1))
(Evaluation (Predicate "has nose") (Concept "dog") (CountTruthValue 1 0 2))
(Evaluation (Predicate "has tail") (Concept "dog") (CountTruthValue 1 0 3))
(Evaluation (Predicate "furry")    (Concept "dog") (CountTruthValue 1 0 4))
(Evaluation (Predicate "domestic") (Concept "dog") (CountTruthValue 1 0 5))

(Evaluation (Predicate "has legs") (Concept "cat") (CountTruthValue 1 0 1))
(Evaluation (Predicate "has nose") (Concept "cat") (CountTruthValue 1 0 2))
(Evaluation (Predicate "has tail") (Concept "cat") (CountTruthValue 1 0 3))
(Evaluation (Predicate "furry")    (Concept "cat") (CountTruthValue 1 0 4))
(Evaluation (Predicate "domestic") (Concept "cat") (CountTruthValue 1 0 5))


(define qdot-math
	(Query
		(TypedVariable (Variable "$prop") (Type 'Predicate))
		(Present
			(Evaluation (Variable "$prop") (Concept "dog"))
			(Evaluation (Variable "$prop") (Concept "cat")))
		(Times
			(CountOf (Evaluation (Variable "$prop") (Concept "dog")))
			(CountOf (Evaluation (Variable "$prop") (Concept "cat"))))))

(cog-execute! qdot-math)

(cog-execute! (Accumulate qdot-math))

