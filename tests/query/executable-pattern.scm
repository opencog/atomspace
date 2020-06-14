;
; Data and tests for Executable Pattern
;

(use-modules (opencog) (opencog exec))

(PlusLink (NumberNode 3) (NumberNode 5))

(define plus-pattern
	(GetLink
		(VariableList
			(TypedVariableLink (VariableNode "$A") (TypeNode "NumberNode"))
			(TypedVariableLink (VariableNode "$B") (TypeNode "NumberNode")))
		(Present (PlusLink (VariableNode "$A") (VariableNode "$B")))))

; (cog-execute! plus-pattern)
