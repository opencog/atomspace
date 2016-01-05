
(use-modules (opencog) (opencog exec))

(define single
	(MapLink
		(ScopeLink
			(Variable "$x")
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(EvaluationLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Concept "baz"))))
)

(define single-set
	(MapLink
		(ScopeLink
			(Variable "$x")
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(SetLink
			; Not in alphaebtical or type-order!
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
		))
)

(define single-list
	(MapLink
		(ScopeLink
			(Variable "$x")
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(ListLink
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
		))
)

(define single-type
	(MapLink
		(ScopeLink
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(SetLink
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
		))
)
