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
; ----------------------------------

(define bindli
	(Bind (VariableList
			(Variable "$x")
			(Anchor "bind-results"))
		(Present
			(Evaluation (Predicate "foo")
				(List (Variable "$x") (Concept "alpha"))))
		(Inheritance (Variable "$x") (Concept "letters"))))

; (cog-execute! bindli)
