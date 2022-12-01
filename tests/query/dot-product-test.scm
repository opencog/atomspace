;
; dot-product.scm -- take dot products of vectors for query
; This is the unit-test version of one of the demos.
;
; The QueryLink not only performs searches, but it can apply rewrite
; rules to the results. These re-write rules can generate (numeric)
; Values, which are returned to the caller.  This example illustrates
; a query that returns two vectors of numbers, which are multiplied
; and accumulated, resulting in a dot-product of the two.

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "dot-product-test")
(test-begin tname)

; Define a pair of vectors. One vector is called "dog", the other is
; called "cat". The basis elements of both vectors are "has legs",
; "has nose" and so on. The numeric value for that basis element is
; stored in a CountTruthValue attached to each.
;
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
; "cat" vectors. Once these are found, obtain the counts, and multiply
; them together.
(define qdot-math
	(Query
		; The search variable.
		(TypedVariable (Variable "$prop") (Type 'Predicate))

		; What to look for.
		(Present
			(Evaluation (Variable "$prop") (Concept "dog"))
			(Evaluation (Variable "$prop") (Concept "cat")))

		; Multiply the counts on the search results.
		(Times
			(CountOf (Evaluation (Variable "$prop") (Concept "dog")))
			(CountOf (Evaluation (Variable "$prop") (Concept "cat"))))))

; Dry run -- this should return a list of numbers 1,4,9,16,25
; (cog-execute! qdot-math)

; Accumulate the numeric values: this should return 1+4+9+16+25 = 55.
(define five-five (cog-execute! (Accumulate qdot-math)))

(test-assert "final dot product" (equal? (FloatValue 55) five-five))

(test-end tname)

(opencog-test-end)
