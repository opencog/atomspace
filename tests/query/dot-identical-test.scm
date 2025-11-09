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

(define tvkey (Predicate "*-TruthValueKey-*"))
(define (count-of ATOM) (ElementOf (Number 2) (ValueOf ATOM tvkey)))

(cog-set-value! (Edge (Predicate "has legs") (Concept "dog")) tvkey (FloatValue 1 0 1))
(cog-set-value! (Edge (Predicate "has nose") (Concept "dog")) tvkey (FloatValue 1 0 2))
(cog-set-value! (Edge (Predicate "has tail") (Concept "dog")) tvkey (FloatValue 1 0 3))
(cog-set-value! (Edge (Predicate "furry")    (Concept "dog")) tvkey (FloatValue 1 0 4))
(cog-set-value! (Edge (Predicate "domestic") (Concept "dog")) tvkey (FloatValue 1 0 5))

(cog-set-value! (Edge (Predicate "has legs") (Concept "cat")) tvkey (FloatValue 1 0 1))
(cog-set-value! (Edge (Predicate "has nose") (Concept "cat")) tvkey (FloatValue 1 0 2))
(cog-set-value! (Edge (Predicate "has tail") (Concept "cat")) tvkey (FloatValue 1 0 3))
(cog-set-value! (Edge (Predicate "furry")    (Concept "cat")) tvkey (FloatValue 1 0 4))
(cog-set-value! (Edge (Predicate "domestic") (Concept "cat")) tvkey (FloatValue 1 0 5))

; Define a Query that looks for the basis elements on the "dog" and
; "cat" vectors. Once these are found, obtain the counts, and multiply
; them together.
(define qdot-math
	(Query
		; The search variable.
		(VariableList
			(TypedVariable (Variable "$prop") (Type 'Predicate))
			(TypedVariable (Variable "$dog") (Type 'Edge))
			(TypedVariable (Variable "$cat") (Type 'Edge))
		)

		(And
			; We can explicitly ask for presence, but it should be
			; enough to implicitly assume it, with the IdenticalLink.
			;(Present
			;	(Edge (Variable "$prop") (Concept "dog"))
			;	(Edge (Variable "$prop") (Concept "cat")))
			(Identical (Variable "$dog")
				(Edge (Variable "$prop") (Concept "dog")))
			(Identical (Variable "$cat")
				(Edge (Variable "$prop") (Concept "cat")))
		)

		; Multiply the counts on the search results.
		(Times
			(count-of (Variable "$dog"))
			(count-of (Variable "$cat")))))

; Accumulate the numeric values: this should return 1+4+9+16+25 = 55.
(define five-five (cog-execute! (Accumulate qdot-math)))

(test-assert "final dot product" (equal? (FloatValue 55) five-five))

(test-end tname)

(opencog-test-end)
