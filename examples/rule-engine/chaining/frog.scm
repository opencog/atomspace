(define rule1
	(BindLink
		(VariableList
			(VariableNode "$x")
		)
		(AndLink
			(InheritanceLink
				(VariableNode "$x")
				(ConceptNode "croaks")
			)
			(EvaluationLink
				(PredicateNode "eats")
				(ListLink
					(VariableNode "$x")
					(ConceptNode "flies")
				)
			)
		)
		(InheritanceLink
			(VariableNode "$x")
			(ConceptNode "frog")
		)
	)
)

(define rule1-name (Node "rule1"))
(DefineLink rule1-name rule1)

(define rule2
	(BindLink
		(VariableList
			(VariableNode "$x")
		)
		(InheritanceLink
			(VariableNode "$x")
			(ConceptNode "frog")
		)
		(InheritanceLink
			(VariableNode "$x")
			(ConceptNode "green")
		)
	)
)

(define rule2-name (Node "rule2"))
(DefineLink rule2-name rule2)

(define source
	(InheritanceLink
		(ConceptNode "fritz")
		(ConceptNode "croaks")
	)
)

(EvaluationLink
	(PredicateNode "eats")
	(ListLink
		(ConceptNode "fritz")
		(ConceptNode "flies")
	)
)

;-------------------------------------------
(define wiki (ConceptNode "wikipedia-fc"))

(InheritanceLink  ; Defining a rule base
	(ConceptNode "wikipedia-fc")
	(ConceptNode "URE")
)

(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "wikipedia-fc")
   (NumberNode 20)
)

(MemberLink (stv 0.9 1)
	rule1-name
	(ConceptNode "wikipedia-fc")
)

(MemberLink (stv 0.5 1)
	rule2-name
	(ConceptNode "wikipedia-fc")
)
