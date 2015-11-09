;
; Unit testing for a strange repeated-clause
;
;;; Populate the atomspace with two things
(use-modules (opencog))
(use-modules (opencog query))

; Two parts, both identical
(ImplicationLink
	(ListLink
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
	)
	(ListLink
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
	)
)

;; Two parts, one different than the other, but having a common
;; subexpression.
(ImplicationLink
	(ListLink
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
	)
	(MemberLink
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
	)
)

;;; Note that the evaluationLink is repeated twice, inside of
;;; a ListLink, each time.
(define (repeat-same)
	(BindLink
		(VariableNode "$x")
		(ImplicationLink
			(ListLink
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
			)
			(ListLink
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
			)
		)
		(VariableNode "$x")
	)
)

;;; Note that the evaluationLink is repeated twice, inside of
;;; two different links
(define (repeat-different)
	(BindLink
		(VariableNode "$x")
		(ImplicationLink
			(ListLink
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
			)
			(MemberLink
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
			)
		)
		(VariableNode "$x")
	)
)
