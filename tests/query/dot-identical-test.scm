;
; dot-identical-test.scm -- take dot products of vectors for query
; See `dot-product-test.scm` for the basic version and explanation.
; Variant using IdenticalLinks.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "dot-identical-test")
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
		(VariableList
			(TypedVariable (Variable "$prop") (Type 'Predicate))
			(TypedVariable (Variable "$dog") (Type 'Evaluation))
			(TypedVariable (Variable "$cat") (Type 'Evaluation))
		)

		; What to look for.
		(And
			(Present
				(Evaluation (Variable "$prop") (Concept "dog"))
				(Evaluation (Variable "$prop") (Concept "cat")))
			(Identical (Variable "$dog")
				(Evaluation (Variable "$prop") (Concept "dog")))
			(Identical (Variable "$cat")
				(Evaluation (Variable "$prop") (Concept "cat")))
		)

		; Multiply the counts on the search results.
		(Times
			(CountOf (Variable "$dog"))
			(CountOf (Variable "$cat")))))

; Accumulate the numeric values: this should return 1+4+9+16+25 = 55.
(define five-five (cog-execute! (Accumulate qdot-math)))

(test-assert "final dot product" (equal? (FloatValue 55) five-five))

(test-end tname)
