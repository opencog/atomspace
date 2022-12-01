;
; dot-choice.scm -- take dot products of vectors for query
; See `dot-product-test.scm` for the basic version and explanation.
; Variant using ChoiceLinks underneath IdenticalLinks.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "dot-choice-test")
(test-begin tname)

(Evaluation (Predicate "has legs") (Concept "dog") (CountTruthValue 1 0 1))
(Evaluation (Predicate "has nose") (Concept "dog") (CountTruthValue 1 0 2))
(Evaluation (Predicate "has tail") (Concept "dog") (CountTruthValue 1 0 3))
(Associative (Predicate "furry")    (Concept "dog") (CountTruthValue 1 0 4))
(Associative (Predicate "domestic") (Concept "dog") (CountTruthValue 1 0 5))

(Evaluation (Predicate "has legs") (Concept "cat") (CountTruthValue 1 0 2))
(Evaluation (Predicate "has nose") (Concept "cat") (CountTruthValue 1 0 3))
(Evaluation (Predicate "has tail") (Concept "cat") (CountTruthValue 1 0 4))
(Associative (Predicate "furry")    (Concept "cat") (CountTruthValue 1 0 5))
(Associative (Predicate "domestic") (Concept "cat") (CountTruthValue 1 0 6))

; Define a Query that looks for the basis elements on the "dog" and
; "cat" vectors. Once these are found, obtain the counts, and multiply
; them together.
(define qdot-math
	(Query
		; The search variable.
		(VariableList
			(TypedVariable (Variable "$prop") (Type 'Predicate))
			(TypedVariable (Variable "$dog")
				(TypeChoice (Type 'Evaluation) (Type 'Associative)))
			(TypedVariable (Variable "$cat")
				(TypeChoice (Type 'Evaluation) (Type 'Associative)))
		)

		; What to look for.
		(And
			(Identical (Variable "$dog")
				(Choice
					(Evaluation (Variable "$prop") (Concept "dog"))
					(Associative (Variable "$prop") (Concept "dog"))))
			(Identical (Variable "$cat")
				(Choice
					(Evaluation (Variable "$prop") (Concept "cat"))
					(Associative (Variable "$prop") (Concept "cat"))))
		)

		; Multiply the counts on the search results.
		(Times
			(CountOf (Variable "$dog"))
			(CountOf (Variable "$cat")))))

; Accumulate the numeric values: this should return 2+6+12+20+30 = 70.
(define seventy (cog-execute! (Accumulate qdot-math)))

(test-assert "final dot product" (equal? (FloatValue 70) seventy))

(test-end tname)

(opencog-test-end)
