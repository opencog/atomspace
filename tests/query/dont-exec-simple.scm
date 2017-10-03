
(use-modules (opencog) (opencog exec))

(define bad-and (AndLink (Concept "a") (Concept "b")))

; Evaluating the below should just unwrap the DontExec
(define dont-and (DontExec bad-and))

; Evaluating the below should just return the bad-and
(define put-and
	(Put (DontExec (AndLink (Variable "$x")(Variable "$y")))
		(ListLink (Concept "a") (Concept "b"))))
