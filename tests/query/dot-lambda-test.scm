;
; dot-lambda-test.scm -- take dot products of vectors for query
; See `dot-product-test.scm` for the base cse.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "dot-lambda-test")
(test-begin tname)

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

; Define a Query that looks for the basis elements on the "dog" and
; "cat" vectors. Once these are found, obtain the counts, and mutiply
; them together.
(define qdot-math
	(Query
		; The search variable.
		(TypedVariable (Variable "$prop") (Type 'Predicate))

		; What to look for.
		(Present
			(Evaluation (Variable "$prop") (Concept "dog"))
			(Evaluation (Variable "$prop") (Concept "cat")))

		; Apply a function to the results of the search
		(Put
			(Lambda (Variable "$foo")
				(Times
					(CountOf (Evaluation (Variable "$foo") (Concept "dog")))
					(CountOf (Evaluation (Variable "$foo") (Concept "cat")))))
			(Variable "$prop"))))

; Dry run -- this should return a list of numbers 1,4,9,16,25
; (cog-execute! qdot-math)

; Accumulate the numeric values: this should return 1+4+9+16+25 = 55.
(define five-five (cog-execute! (Accumulate qdot-math)))

(test-assert "final dot product" (equal? (FloatValue 55) five-five))

(test-end tname)
