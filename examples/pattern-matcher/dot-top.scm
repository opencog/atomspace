;
; dot-top.scm -- take dot products of vectors for query
; Development/test version.
;
; The QueryLink not only performs searches, but it can apply rewrite
; rules to the results. These re-write rules can generate (numeric)
; Values, which are returned to the caller.  This example illustrates
; a query that returns two vectors of numbers, which are multiplied
; and accumulated, resulting in a dot-product of the two.

(use-modules (opencog) (opencog exec))

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

(use-modules (opencog logger))
(cog-logger-set-stdout! #t)
(cog-logger-set-sync! #t)
(cog-logger-set-level! "FINE")

; Dry run -- this should return a list of numbers 1,4,9,16,25
(cog-execute! qdot-math)

; Accumulate the numeric values: this should return 1+4+9+16+25 = 55.
; (cog-execute! (Accumulate qdot-math))

; That's all folks! The End.
