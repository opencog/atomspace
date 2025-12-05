;
; Unit testing for addition of "dummy" clauses into pattern.
;
(use-modules (opencog))

; Data
(Number 42)
(Number 3003)

; Query. Should return only "42"
(define dummy
	(MeetLink
		(TypedVariable (Variable "$x") (Type "NumberNode"))
		(GreaterThan (Number 88) (Variable "$x"))))
