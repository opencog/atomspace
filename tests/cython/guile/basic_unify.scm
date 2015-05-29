;
; Basic test of unification
;
; Create some misc atoms.

(add-to-load-path "./opencog/scm")
(use-modules (opencog))
(use-modules (opencog query))

(define (stv mean conf) (cog-new-stv mean conf))

(ListLink
	(ConceptNode "hello" (stv 0.5 0.5))
	(ConceptNode "world")
)

(define wobbly
	(ConceptNode "wobbly" (stv 0.5 0.5))
)

(EvaluationLink
	(PredicateNode "of")
	(ListLink
		(ConceptNode "capital@ggg")
		(ConceptNode "Germany")
	)
)
(EvaluationLink
	(PredicateNode "of")
	(ListLink
		(ConceptNode "capital@fff")
		(ConceptNode "France")
	)
)
(EvaluationLink
	(PredicateNode "_subj")
	(ListLink
		(ConceptNode "be@fff")
		(ConceptNode "capital@fff")
	)
)
(EvaluationLink
	(PredicateNode "_obj")
	(ListLink
		(ConceptNode "be@fff")
		(ConceptNode "Paris")
	)
)
(EvaluationLink
	(PredicateNode "_subj")
	(ListLink
		(ConceptNode "be@ggg")
		(ConceptNode "capital@ggg")
	)
)
(EvaluationLink
	(PredicateNode "_obj")
	(ListLink
		(ConceptNode "be@ggg")
		(ConceptNode "Berlin")
	)
)

; Given subject, object and preposition, deduce the capital.
;
(define impl
	(ImplicationLink
		(AndLink
			(EvaluationLink
				(PredicateNode "_subj")
				(ListLink
					(VariableNode "$be-word")
					(VariableNode "$capital")
				)
			)
			(EvaluationLink
				(PredicateNode "_obj")
				(ListLink
					(VariableNode "$be-word")
					(VariableNode "$city")
				)
			)
			(EvaluationLink
				(PredicateNode "of")
				(ListLink
					(VariableNode "$capital")
					(VariableNode "$country")
				)
			)
		)
		(EvaluationLink
			(PredicateNode "capital-of")
			(ListLink
				(VariableNode "$country")
				(VariableNode "$city")
			)
		)
	)
)

; The actual BindLink
(define cap-deduce
	(BindLink
		(VariableList
			(VariableNode "$be-word")
			(VariableNode "$capital")
			(VariableNode "$country")
			(VariableNode "$city")
		)
		impl
	)
)

