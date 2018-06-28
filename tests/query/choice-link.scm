;
; Basic unit testing for ChoiceLinks in the pattern matcher.
;
(use-modules (opencog))
(use-modules (opencog exec))

;;; Populate the atomspace with four small trees.
(MemberLink
	(ConceptNode "ways and means")
	(ConceptNode "Tom")
)

(MemberLink
	(ConceptNode "ways and means")
	(ConceptNode "Joe")
)

(MemberLink
	(ConceptNode "ways and means")
	(ConceptNode "Hank")
)

(MemberLink
	(ConceptNode "Senator")
	(ConceptNode "Tom")
)

(MemberLink
	(ConceptNode "Representative")
	(ConceptNode "Joe")
)

;; We should NOT find Hank among the solutions
(MemberLink
	(ConceptNode "CEO")
	(ConceptNode "Hank")
)

;;; Two clauses; they both connected with a common variable.
(define (basic)
	(BindLink
		(AndLink
			(MemberLink
				(ConceptNode "ways and means")
				(VariableNode "$x")
			)
			(ChoiceLink
				(MemberLink
					(ConceptNode "Senator")
					(VariableNode "$x")
				)
				(MemberLink
					(ConceptNode "Representative")
					(VariableNode "$x")
				)
			)
		)
		(VariableNode "$x")
	)
)
