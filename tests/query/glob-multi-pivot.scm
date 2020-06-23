;
; glob-multi-pivot.scm
;
; multi-pivot version of glob-pivot.scm
; 
;
(use-modules (opencog) (opencog exec))

(EvaluationLink
	(PredicateNode "pred-1")
	(ListLink (ConceptNode "blah")))

(EvaluationLink
	(PredicateNode "pred-2")
	(ListLink (ConceptNode "blah")))

(MemberLink
	(OrderedLink (ConceptNode "blah"))
	(ConceptNode "class-1"))

(MemberLink
	(OrderedLink (ConceptNode "blah"))
	(ConceptNode "class-2"))

(MemberLink
	(ConceptNode "blah")
	(ConceptNode "class-3"))

; ---------------------------------------------

(define glob-unify-plain
	(GetLink
		(TypedVariableLink
			(GlobNode "$G")
			(TypeSetLink
				(TypeNode "ConceptNode")
				(IntervalLink (NumberNode 1) (NumberNode -1))
			)
		)
		(AndLink
			(EvaluationLink
				(PredicateNode "pred-1")
				(ListLink (GlobNode "$G")))

			(MemberLink
				(GlobNode "$G")
				(ConceptNode "class-3"))
		)
	)
)

(define glob-unify-tall
	(GetLink
		(TypedVariableLink
			(GlobNode "$G")
			(TypeSetLink
				(TypeNode "ConceptNode")
				(IntervalLink (NumberNode 1) (NumberNode -1))
			)
		)
		(AndLink
			(EvaluationLink
				(PredicateNode "pred-1")
				(ListLink (GlobNode "$G")))

			(MemberLink
				(OrderedLink (GlobNode "$G"))
				(ConceptNode "class-1"))
		)
	)
)

; ---------------------------------------------

(define glob-chase-pivot
	(GetLink
		(VariableList
			(Variable "$cls")
			(TypedVariableLink
				(GlobNode "$G")
				(TypeSetLink
					(TypeNode "ConceptNode")
					(IntervalLink (NumberNode 1) (NumberNode -1))
				)
			)
		)
		(AndLink
			(EvaluationLink
				(PredicateNode "pred-1")
				(ListLink (GlobNode "$G")))

			(EvaluationLink
				(PredicateNode "pred-2")
				(ListLink (GlobNode "$G")))

			(MemberLink
				(GlobNode "$G")
				(Variable "$cls"))
		)
	)
)

; ---------------------------------------------
(define glob-multi-pivot
	(GetLink
		(TypedVariableLink
			(GlobNode "$G")
			(TypeSetLink
				(TypeNode "ConceptNode")
				(IntervalLink (NumberNode 1) (NumberNode -1))
			)
		)
		(AndLink
			(EvaluationLink
				(PredicateNode "pred-1")
				(ListLink (GlobNode "$G")))

			(EvaluationLink
				(PredicateNode "pred-2")
				(ListLink (GlobNode "$G")))

			(MemberLink
				(OrderedLink (GlobNode "$G"))
				(ConceptNode "class-1"))

			(MemberLink
				(OrderedLink (GlobNode "$G"))
				(ConceptNode "class-2"))
		)
	)
)

; ---------------------------------------------
(define glob-chase-multi-pivot
	(GetLink
		(VariableList
			(Variable "$cls")
			(TypedVariableLink
				(GlobNode "$G")
				(TypeSetLink
					(TypeNode "ConceptNode")
					(IntervalLink (NumberNode 1) (NumberNode -1))
				)
			)
		)
		(AndLink
			(EvaluationLink
				(PredicateNode "pred-1")
				(ListLink (GlobNode "$G")))

			(EvaluationLink
				(PredicateNode "pred-2")
				(ListLink (GlobNode "$G")))

			(MemberLink
				(OrderedLink (GlobNode "$G"))
				(ConceptNode "class-1"))

			(MemberLink
				(GlobNode "$G")
				(Variable "$cls"))
		)
	)
)
