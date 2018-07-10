
(use-modules (opencog) (opencog exec))

(define bad-and (AndLink (Concept "a") (Concept "b")))

; Evaluating the below should just unwrap the DontExec
(define dont-and (DontExec bad-and))

; Evaluating the below should just return the bad-and
(define put-and
	(Put (DontExec (AndLink (Variable "$x")(Variable "$y")))
		(ListLink (Concept "a") (Concept "b"))))

; A multi-argument put whose EvaluationLink should not evaluate
(define put-eval
	(Put (DontExec (EvaluationLink
			(PredicateNode "foo")
			(Variable "$x")
			(Variable "$y")
			(Variable "$z")))
		(SetLink
			(ListLink (Concept "a") (Concept "b") (Concept "c"))
			(ListLink (Concept "d") (Concept "e") (Concept "f"))
			(ListLink (Concept "g") (Concept "h") (Concept "i"))
			(ListLink (Concept "j") (Concept "k") (Concept "l")))))

; The result we expect from the above
(define eval-expected (SetLink
	(Evaluation (Predicate "foo") (Concept "a") (Concept "b") (Concept "c"))
	(Evaluation (Predicate "foo") (Concept "d") (Concept "e") (Concept "f"))
	(Evaluation (Predicate "foo") (Concept "g") (Concept "h") (Concept "i"))
	(Evaluation (Predicate "foo") (Concept "j") (Concept "k") (Concept "l"))))
