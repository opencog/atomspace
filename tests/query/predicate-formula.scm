;
; predicate-formula.scm
; Make sure that PredicateFormulaLink works in search patterns.
; This tests the second bug reported in opencog/atomspace#2650
;
(use-modules (opencog) (opencog exec))

(Member
	(Evaluation
		(Predicate "has_name")
		(List (Concept "node1") (Concept "name1")))
	(Concept "node2"))

; The answer we always expet to get:
(define ans (List (Concept "node1") (Concept "name1")))

; Make sure this passes, before we get fancy
(define q-basic (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Equal (Variable "Y")
			(List (Variable "N") (Concept "name1"))))
  (Variable "Y")))

; Same as above but with IdenticalLink
(define qi-basic (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Identical (Variable "Y")
			(List (Variable "N") (Concept "name1"))))
  (Variable "Y")))

(define qe1 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(Evaluation
			(PredicateFormula
				(Minus (Number 1)
					(Times
						(StrengthOf (Variable "$X"))
						(StrengthOf (Variable "$Y"))))
				(Times
					(ConfidenceOf (Variable "$X"))
					(ConfidenceOf (Variable "$Y"))))
			(Variable "Y")))
  (Variable "Y")))

; (cog-execute! qe1)

; Same as above, but with multiple components (equal link)
(define qe2 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Equal (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		(Evaluation
			(PredicateFormula
				(Minus (Number 1)
					(Times
						(StrengthOf (Variable "$X"))
						(StrengthOf (Variable "$Y"))))
				(Times
					(ConfidenceOf (Variable "$X"))
					(ConfidenceOf (Variable "$Y"))))
			(Variable "Y")))
  (Variable "Y")))

; (cog-execute! qe2)

; Same as above, but with IdenticalLink
(define qe2i (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Identical (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		(Evaluation
			(PredicateFormula
				(Minus (Number 1)
					(Times
						(StrengthOf (Variable "$X"))
						(StrengthOf (Variable "$Y"))))
				(Times
					(ConfidenceOf (Variable "$X"))
					(ConfidenceOf (Variable "$Y"))))
			(Variable "Y")))
  (Variable "Y")))

; (cog-execute! qe2i)

(define qe3 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Equal (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		(Evaluation
			(DefinedPredicate "pred1")
			(Variable "Y")))
  (Variable "Y")))

; Same as above, but with Identical
(define qe3i (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(Identical (Variable "Y")
			(List (Variable "N") (Concept "name1")))
		(Evaluation
			(DefinedPredicate "pred1")
			(Variable "Y")))
  (Variable "Y")))

; The definition needed for the above.
(DefineLink
	(DefinedPredicate "pred1")
	(PredicateFormula
		(Minus (Number 1)
			(Times
				(StrengthOf (Variable "$X"))
				(StrengthOf (Variable "$Y"))))
		(Times
			(ConfidenceOf (Variable "$X"))
			(ConfidenceOf (Variable "$Y")))))

; (cog-execute! qe3)

; Like above, but without the EqualLink
(define qe4 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(Evaluation
			(DefinedPredicate "pred1")
			(Variable "Y")))
  (Variable "Y")))

; (cog-execute! qe4)


; A simpler more direct version
(define qe5 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(GreaterThan
			(StrengthOf (Variable "Y")) (Number 0.5)))
  (Variable "Y")))

; (cog-execute! qe5)

; A convoluted version of above.
(define qe6 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(GreaterThan
			(StrengthOf
				(Evaluation
					(DefinedPredicate "pred1")
					(Variable "Y")))
			(Number 0.5)))
  (Variable "Y")))

; (cog-execute! qe6)

; Finally, verbose and convoluted
(define qe7 (Query
	(And
		(Member
			(Evaluation (Predicate "has_name") (Variable "Y"))
			(Concept "node2"))
		(TypedVariable (Variable "Y")
			(Signature (List (Type 'Concept) (Concept "name1"))))
		(GreaterThan
			(StrengthOf
				(Evaluation
					(PredicateFormula
						(Minus (Number 1)
							(Times
								(StrengthOf (Variable "$X"))
								(StrengthOf (Variable "$Y"))))
						(Times
							(ConfidenceOf (Variable "$X"))
							(ConfidenceOf (Variable "$Y"))))
					(Variable "Y")))
			(Number 0.5)))
  (Variable "Y")))

; (cog-execute! qe7)
