;
; Unit testing for a constant pattern
;
(use-modules (opencog))
(use-modules (opencog query) (opencog exec))

; Data
(Number 42)
(Number 3003)

; Query. Should return only "42"
(define dummy
	(GetLink
		(TypedVariable (Variable "$x") (Type "NumberNode"))
		(GreaterThan (Number 88) (Variable "$x"))))
