(use-modules (opencog) (opencog exec))

; A lot like put-get-multi.scm, but the Gets have been previously run.

(define put-multi-set
	(PutLink
		(LambdaLink
			(VariableList (Variable "x") (Variable "y"))
			(EvaluationLink
				(PredicateNode "relatives")
				(ListLink
					(Variable "x")
					(Variable "y")
					(Concept "mom and pop"))))
		(ListLink
			(SetLink (Concept "Jim"))
			(SetLink (Concept "Jane")))))
