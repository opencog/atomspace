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

(define tvkey (Predicate "*-TruthValueKey-*"))
(define (count-of ATOM) (ElementOf (Number 2) (ValueOf ATOM tvkey)))

(cog-set-value! (Edge (Predicate "has legs") (Concept "dog")) tvkey (FloatValue 1 0 1))
(cog-set-value! (Edge (Predicate "has nose") (Concept "dog")) tvkey (FloatValue 1 0 2))
(cog-set-value! (Edge (Predicate "has tail") (Concept "dog")) tvkey (FloatValue 1 0 3))
(cog-set-value! (Associative (Predicate "furry")    (Concept "dog")) tvkey (FloatValue 1 0 4))
(cog-set-value! (Associative (Predicate "domestic") (Concept "dog")) tvkey (FloatValue 1 0 5))

(cog-set-value! (Edge (Predicate "has legs") (Concept "cat")) tvkey (FloatValue 1 0 2))
(cog-set-value! (Edge (Predicate "has nose") (Concept "cat")) tvkey (FloatValue 1 0 3))
(cog-set-value! (Edge (Predicate "has tail") (Concept "cat")) tvkey (FloatValue 1 0 4))
(cog-set-value! (Associative (Predicate "furry")    (Concept "cat")) tvkey (FloatValue 1 0 5))
(cog-set-value! (Associative (Predicate "domestic") (Concept "cat")) tvkey (FloatValue 1 0 6))

; Define a Query that looks for the basis elements on the "dog" and
; "cat" vectors. Once these are found, obtain the counts, and multiply
; them together.
(define qdot-math
	(Query
		; The search variable.
		(VariableList
			(TypedVariable (Variable "$prop") (Type 'Predicate))
			(TypedVariable (Variable "$dog")
				(TypeChoice (Type 'Edge) (Type 'Associative)))
			(TypedVariable (Variable "$cat")
				(TypeChoice (Type 'Edge) (Type 'Associative)))
		)

		; What to look for.
		(And
			(Identical (Variable "$dog")
				(Choice
					(Edge (Variable "$prop") (Concept "dog"))
					(Associative (Variable "$prop") (Concept "dog"))))
			(Identical (Variable "$cat")
				(Choice
					(Edge (Variable "$prop") (Concept "cat"))
					(Associative (Variable "$prop") (Concept "cat"))))
		)

		; Multiply the counts on the search results.
		(Times
			(count-of (Variable "$dog"))
			(count-of (Variable "$cat")))))

; Accumulate the numeric values: this should return 2+6+12+20+30 = 70.
(define seventy (cog-execute! (Accumulate qdot-math)))

(test-assert "final dot product" (equal? (FloatValue 70) seventy))

(test-end tname)

(opencog-test-end)
