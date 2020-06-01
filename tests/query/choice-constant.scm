;
; choice-constant.scm
; ChoiceLink with no variables!!
;
(use-modules (opencog))
(use-modules (opencog exec))

;;; Populate the atomspace with four small trees.
(ListLink
	(ConceptNode "A")
	(ConceptNode "B")
)

;;; Two clauses; they both connected with a common variable.
(define query
	(GetLink
		(TypedVariableLink (VariableNode "$x") (TypeNode "ConceptNode"))
		(ListLink
			(ChoiceLink
				(ConceptNode "A")
				(ConceptNode "C")
			)
			(VariableNode "$x")
		)
	)
)
