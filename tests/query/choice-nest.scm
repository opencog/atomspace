;
; Unit testing for nested ChoiceLinks in the pattern matcher.
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
	(ConceptNode "ways and means")
	(ConceptNode "Mary")
)

(MemberLink
	(ConceptNode "ways and means")
	(ConceptNode "Phillip")
)

(MemberLink
	(ConceptNode "ways and means")
	(ConceptNode "Milton")
)

(MemberLink
	(ConceptNode "ways and means")
	(ConceptNode "Charlie")
)

(MemberLink
	(ConceptNode "ways and means")
	(ConceptNode "Chayim")
)

(MemberLink
	(ConceptNode "ways and means")
	(ConceptNode "Stuart")
)

;;; the list link serves o purpose other than to "embed"
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

(ListLink
	(MemberLink
		(ConceptNode "Page")
		(ConceptNode "Mary")
	)
)

(ListLink
	(MemberLink
		(ConceptNode "Secretary")
		(ConceptNode "Phillip")
	)
)

(ListLink
	(EvaluationLink
		(PredicateNode "involved")
		(ListLink
			(ConceptNode "Milton")
			(ConceptNode "Business")
		)
	)
)

(ListLink
	(EvaluationLink
		(PredicateNode "involved")
		(ListLink
			(ConceptNode "Charlie")
			(ConceptNode "Industry")
		)
	)
)

(ListLink
	(EvaluationLink
		(PredicateNode "involved")
		(ListLink
			(ConceptNode "Chayim")
			(ConceptNode "Banking")
		)
	)
)

;; Should NOT find Stuart
(ListLink
	(EvaluationLink
		(PredicateNode "involved")
		(ListLink
			(ConceptNode "Stuart")
			(ConceptNode "Diletant")
		)
	)
)

;;; Nested clauses; all connected with a common variable.
(define (nest)
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
					(EvaluationLink
						(PredicateNode "involved")
						(ChoiceLink
							(ListLink
								(VariableNode "$x")
								(ConceptNode "Business")
							)
							(ListLink
								(VariableNode "$x")
								(ConceptNode "Industry")
							)
							(ListLink
								(VariableNode "$x")
								(ConceptNode "Banking")
							)
						)
					)
				)
			)
		)
		(VariableNode "$x")
	)
)

;; Simple nesting -- Or within Or 
(define (nest-bad)
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
					;;  Note this Or within an Or
					(ChoiceLink
						(MemberLink
							(ConceptNode "Page")
							(VariableNode "$x")
						)
						(MemberLink
							(ConceptNode "Secretary")
							(VariableNode "$x")
						)
					)
				)
			)
		)
		(VariableNode "$x")
	)
)
