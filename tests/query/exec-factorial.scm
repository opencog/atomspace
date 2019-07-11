
(use-modules (opencog) (opencog exec))

; Compute the factorial of a NumberNode, using the classic
; recursive algorithm.
;
(Define
	(DefinedSchema "factorial")
	(Lambda
		(TypedVariable (Variable "$n") (Type "NumberNode"))
		(Cond
			(GreaterThan (Variable "$n") (Number 0))
			(Times
				(Variable "$n")
				(ExecutionOutput
					(DefinedSchema "factorial")
					(Minus (Variable "$n") (Number 1))))
			(Number 1)))
)

; (cog-execute! (ExecutionOutput (DefinedSchema "factorial") (Number 5)))
