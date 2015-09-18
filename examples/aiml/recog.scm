
; Some very crude experiments

(define data
	(PatternLink
		(ListLink
			(ConceptNode "hello")
			(ConceptNode "world"))))

(GetLink
	(ListLink
		(ConceptNode "hello")
		(VariableNode "$person")))

(cog-recognize data)
