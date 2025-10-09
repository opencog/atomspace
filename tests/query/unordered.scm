
;;
;; unordered.scm
;;
;; A very very simple test for unordered-link matching.
;; This test just has one single unordered link (the similarity link)
;; and looks for a match for it.  This being the simplest test, and
;; should be easy to pass.
;; 
(SimilarityLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(ExecutionLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink)
	)
)

;; We expect the below to match in two different possible ways:
;; $var_one might bind to either GroundedSchemaNode or the 
;; NumberNode, while $var_two will bind to the other one.
(define (pair)
	(CollectionOf
	(QueryLink
		;; variable decls
		(VariableList
			(VariableNode "$var_one")
			(VariableNode "$var_two")
		)
		;; body
		(SimilarityLink
			(VariableNode "$var_one")
			(VariableNode "$var_two")
			(ExecutionLink
				(GroundedSchemaNode "ActivationModulatorUpdater")
				(ListLink)
			)
		)
		;; implicand -- result
		(ListLink
			(VariableNode "$var_one")
			(VariableNode "$var_two")
		)
	)
	)
)

;; We expect the below to match in only one way: the grounding
;; of $var_schema must be to GroundedSchemaNode, due to it's
;; presence in the ExecutionLink.  Thus, $var_number must,
;; by elimination, be grounded by the NumberNode.
(define (blink)
	(CollectionOf
	(QueryLink
		;; variable decls
		(VariableList
			(VariableNode "$var_number")
			(VariableNode "$var_schema")
		)
		;; body
		(SimilarityLink
			(VariableNode "$var_schema")
			(VariableNode "$var_number")
			(ExecutionLink
				(VariableNode "$var_schema")
				(ListLink)
			)
		)
		;; implicand -- result
		(ListLink
			(VariableNode "$var_number")
			(VariableNode "$var_schema")
		)
	)
	)
)

;; Same as above, except that we reverse order of the two
;; variables in the body.  Despite this, the one valid grounding
;; must be found.
(define (blinker)
	(CollectionOf
	(QueryLink
		;; variable decls
		(VariableList
			(VariableNode "$vbr_schema")
			(VariableNode "$vbr_number")
		)
		;; body
		(SimilarityLink
			(VariableNode "$vbr_schema")
			(VariableNode "$vbr_number")
			(ExecutionLink
				(VariableNode "$vbr_schema")
				(ListLink)
			)
		)
		;; implicand -- result
		(ListLink
			(VariableNode "$vbr_number")
			(VariableNode "$vbr_schema")
		)
	)
	)
)

