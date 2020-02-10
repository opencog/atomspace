;
; AnchorLink unit test
;
(use-modules (opencog) (opencog exec))

(Evaluation (Predicate "foo") (List (Concept "A") (Concept "alpha")))
(Evaluation (Predicate "foo") (List (Concept "B") (Concept "alpha")))
(Evaluation (Predicate "foo") (List (Concept "C") (Concept "alpha")))

(define getli
	(Get (VariableList
			(Variable "$x")
			(Anchor "get-results"))
		(Present
			(Evaluation (Predicate "foo")
				(List (Variable "$x") (Concept "alpha"))))))

; (cog-execute! getli)
