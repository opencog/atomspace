;

(use-modules (opencog))
(use-modules (opencog exec))

(define est
	(GetLink

		(VariableList

			(TypedVariable (Variable "$vA") (Type 'Concept))
			(TypedVariable (Variable "$vB") (Type 'Predicate))
			(TypedVariable (Variable "$vC") (Type 'Any))

			(Variable "$vD")
			(Glob "rest")
		)

		(Unordered
			(Unordered (Variable "$vB") (Variable "$vA"))
			(Unordered (Variable "$vC") (Variable "$vB"))
			(Unordered (Variable "$vD") (Variable "$vC"))
			(Glob "rest")
		)
	)
)

(Unordered
	(Unordered (Predicate "B") (Concept "A"))
	(Unordered (Any "C") (Predicate "B"))
	(Unordered (Procedure "D") (Any "C"))
	(Unordered (Anchor "E") (Procedure "D"))
)

; (cog-execute! est)
