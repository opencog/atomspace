;
; Unit testing for a strange repeated-clause
;
;;; Populate the atomspace with two things
(use-modules (opencog))
(use-modules (opencog exec))

; Two parts, both identical
(ListLink
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

; Three parts, all three identical
(ListLink
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

; One part, three identical sub-parts
(ListLink
	(ListLink
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
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
(ListLink
	(ListLink
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
	)
	(MemberLink                     ; TODO this MemberLink is ill-formed
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
	)
)

;; Three parts, all three different, but each having a common
;; subexpression.
(ListLink
	(ListLink
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
	)
	(MemberLink                     ; TODO this MemberLink is ill-formed
		(EvaluationLink
			(PredicateNode "this way")
			(ListLink
				(ConceptNode "this one")
				(ConceptNode "thing two")
			)
		)
	)
	(SubsetLink
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
		(ListLink
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
		(ListLink
			(ListLink
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
			)
			(MemberLink     ; TODO this MemberLink is ill-formed
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

;;; Note that the evaluationLink is repeated three times, inside of
;;; three different links
(define (repeat-diff-thrice)
	(BindLink
		(VariableNode "$x")
		(ListLink
			(ListLink
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
			)
			(MemberLink     ; TODO this MemberLink is ill-formed
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
			)
			(SubsetLink
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

;;; Note that the evaluationLink is repeated three times, inside of
;;; a ListLink, each time.
(define (repeat-thrice)
	(BindLink
		(VariableNode "$x")
		(ListLink
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

;;; Note that the evaluationLink is repeated three times, inside of
;;; just one ListLink, total.
(define (repeat-once)
	(BindLink
		(VariableNode "$x")
		(ListLink
			(ListLink
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
				(EvaluationLink
					(PredicateNode "this way")
					(ListLink
						(VariableNode "$x")
						(ConceptNode "thing two")
					)
				)
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
