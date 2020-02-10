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
; (cog-incoming-by-type (AnchorNode "get-results") 'MemberLink)
; ----------------------------------

(define bindli
	(Bind (VariableList
			(Variable "$z")
			(Anchor "bind-results"))
		(Present
			(Evaluation (Predicate "foo")
				(List (Variable "$z") (Concept "alpha"))))
		(Inheritance (Variable "$z") (Concept "letters"))))

; (cog-execute! bindli)
; (cog-incoming-by-type (AnchorNode "bind-results") 'MemberLink)
; ----------------------------------

; Stimulous-Response-style AI
(define srai
	(Evaluation (Predicate "foo") (List (Concept "C") (Concept "alpha"))))

; (cog-execute! (Dual srai))
