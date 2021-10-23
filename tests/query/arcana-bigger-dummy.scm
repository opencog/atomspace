;
; Unit testing for addition of more complex dummy clauses
;
(use-modules (opencog))
(use-modules (opencog exec))

; Data  -- This is the answer we expect to get.
(define stfu
	(SetLink
		(ListLink
			(ImplicationLink
				(ListLink
					(ConceptNode "I")
					(ConceptNode "love")
					(ConceptNode "you"))
				(ConceptNode "blrable"))
			(ConceptNode "blrable"))))

; Query. Should return the above.
; Note that the query has only a single evaluatable within it.
(define bigger-dummy
	(GetLink
		(VariableList
			(TypedVariable (VariableNode "$whole") (Type "ImplicationLink"))
			(VariableNode "$impl"))
		(Identical
			(VariableNode "$whole")
			(ImplicationLink
				(ListLink
					(ConceptNode "I")
					(ConceptNode "love")
					(ConceptNode "you"))
				(VariableNode "$impl"))
		)
	))

; This is the test to be done:
; (equal? (cog-execute! bigger-dummy) stfu)
