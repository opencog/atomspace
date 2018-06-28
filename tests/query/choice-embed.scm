;
; Unit testing for ChoiceLinks in the pattern matcher.
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

;;; the list link serves no purpose other than to "embed"
(ListLink
	(MemberLink
		(ConceptNode "Senator")
		(ConceptNode "Tom")
	)
)

(ListLink
	(MemberLink
		(ConceptNode "Representative")
		(ConceptNode "Joe")
	)
)

;; We should NOT find Hank!
(ListLink
	(MemberLink
		(ConceptNode "CEO")
		(ConceptNode "Hank")
	)
)

;;; Two clauses; they are both connected with a common variable.
(define (embed)
	(BindLink
		(AndLink
			(MemberLink
				(ConceptNode "ways and means")
				(VariableNode "$x")
			)
			(ListLink
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
		)
		(VariableNode "$x")
	)
)
