;;
;; unordered-more.scm
;;
;; A slightly more complicated unordered-link test case; 
;; has some confounding graphs that should not be found ...

;; should match to this.
(SimilarityLink
	(NumberNode "0.24")
	(ExecutionLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink)
	)
)

;; this should not match.
(SimilarityLink
	(NumberNode "0.24")
	(ExecutionLink
		(SemeNode "We are legion; we are Anonymous")
		(ListLink)
	)
)

;; this should not match.
(SimilarityLink
	(NumberNode "0.24")
	(ExecutionLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(ConceptNode "ring a ling a ding")
		)
	)
)

;; this should not match.
(SimilarityLink
	(NumberNode "0.24")
	(AtTimeLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink)
	)
)

;; this should not match.
(UnorderedLink
	(NumberNode "0.24")
	(ExecutionLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink)
	)
)

;; this should not match.
(SimilarityLink
	(WordNode "0.24")
	(ExecutionLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink)
	)
)

;; Note that the SimilarityLink is unordered ... 
(define (blank)
	(CollectionOf
	(QueryLink
		;; variable decls
		(VariableList
			(TypedVariableLink
				(VariableNode "$var_number_node_type")
				(TypeNode "NumberNode")
			)
		)
		;; body
		(SimilarityLink
			(VariableNode "$var_number_node_type")
			(ExecutionLink
				(GroundedSchemaNode "ActivationModulatorUpdater")
				(ListLink)
			)
		)
		;; implicand -- result
		(ListLink
			(VariableNode "$var_number_node_type")
		)
	)
	)
)

