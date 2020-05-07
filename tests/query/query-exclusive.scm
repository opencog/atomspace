;
; query-exclusive.scm -- ExclusiveLink usage example.
;
(use-modules (opencog) (opencog exec))

(State (Concept "stop light") (Concept "red light"))

; Return empty set, if the light is red.
; In other words, (Exclusive (Concept "red light") (Variable "x"))
; is the same as (Not (Equal (Concept "red light") (Variable "x")))
(define exclude-red
	(Meet
		(Variable "x")
		(And
			(State (Concept "stop light") (Variable "x"))
			(Exclusive (Concept "red light") (Variable "x")))))

(define not-red
	(Meet
		(Variable "x")
		(And
			(State (Concept "stop light") (Variable "x"))
			(Not (Equal (Concept "red light") (Variable "x"))))))
