;
; glob-pivot.scm
;
; Unit test for bug #2167
;
(use-modules (opencog) (opencog exec))

(EvaluationLink
	(PredicateNode "pred-1")
	(ListLink
		(ConceptNode "blah")
	)
)
(EvaluationLink
	(PredicateNode "pred-2")
	(ListLink
		(ConceptNode "blah")
	)
)

(define glob-pivot
	(GetLink
		(TypedVariableLink
			(GlobNode "$G")
			(TypeSetLink
				(TypeNode "ConceptNode")
				(IntervalLink
					(NumberNode 1)
					(NumberNode -1)
				)
			)
		)
		(AndLink
			(EvaluationLink
				(PredicateNode "pred-1")
				(ListLink
					(GlobNode "$G")
				)
			)
			(EvaluationLink
				(PredicateNode "pred-2")
				(ListLink
					(GlobNode "$G")
				)
			)
		)
	)
)
