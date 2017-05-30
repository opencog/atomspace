
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

; Like above but does filtering.
(define single-set-filter
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
				(Predicate "oof dah")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
		))
)

; Same as above, but implicit scoping
(define single-set-noscope
	(MapLink
		(EvaluationLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Variable "$x")))
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

; Like above but does implicit scope and filtering.
(define single-set-filter-noscope
	(MapLink
		(EvaluationLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Variable "$x")))
		(SetLink
			; Not in alphaebtical or type-order!
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
			(EvaluationLink
				(Predicate "oof dah")
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

(define single-signature
	(MapLink
		(ScopeLink
			(TypedVariable (Variable "$x")
				(SignatureLink
					(EvaluationLink
						(Predicate "foo")
						(ListLink (Concept "bar") (Type "ConceptNode")))))
			(Variable "$x"))
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

(define sig-expect
	(SetLink
		(EvaluationLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Concept "ah one")))
		(EvaluationLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Concept "ah two")))
	)
)

(define double-num-set
	(MapLink
		(ScopeLink
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "NumberNode")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y"))))
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

(define double-con-set
	(MapLink
		(ScopeLink
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "ConceptNode")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y"))))
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

;; -------------------------------------------------------------
;; ImplicationLink tests.

(define imply-map
	(MapLink
		(ExtensionalImplicationScopeLink
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "ConceptNode")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y")))
			(EvaluationLink
				(Predicate "reverse-foo")
				(ListLink (Variable "$y") (Variable "$x"))))
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

(define imply-expected
	(SetLink
		(EvaluationLink
			(PredicateNode "reverse-foo")
			(ListLink (ConceptNode "ah one") (ConceptNode "bar")))
		(EvaluationLink
			(PredicateNode "reverse-foo")
			(ListLink (ConceptNode "ah two") (ConceptNode "bar")))
	)
)

(define imply-eval
	(MapLink
		(ExtensionalImplicationScopeLink
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "NumberNode")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y")))
			(EvaluationLink
				(Predicate "reverse-foo nature")
				(ListLink (Variable "$y") (Variable "$x"))))
		(SetLink
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Plus (Number 3) (Number 2))))
		))
)

(define eval-expected
	(SetLink
		(EvaluationLink
			(PredicateNode "reverse-foo nature")
			(ListLink (Number 5) (ConceptNode "bar")))
	)
)

;; -------------------------------------------------------------
;; Implicit-variable ImplicationLink tests.

(define imply-map-nodecl
	(MapLink
		(ExtensionalImplicationScopeLink
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y")))
			(EvaluationLink
				(Predicate "reverse-foo")
				(ListLink (Variable "$y") (Variable "$x"))))
		(SetLink
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "too") (Concept "much")))
		))
)

; Above expects "imply-expected"

(define imply-glob-nodecl
	(MapLink
		(ExtensionalImplicationScopeLink
			(EvaluationLink
				(Predicate "goo")
				(ListLink (Concept "bar") (Glob "$y")))
			(EvaluationLink
				(Predicate "goo nature")
				(ListLink (Concept "gar") (Glob "$y"))))
		(SetLink
			(EvaluationLink
				(Predicate "goo")
				(ListLink (Concept "bar")))
			(EvaluationLink
				(Predicate "goo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EvaluationLink
				(Predicate "goo")
				(ListLink (Concept "bar") (Concept "two") (Concept "three")))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar")))
		))
)

(define imply-glob-expected
	(SetLink
		(EvaluationLink
			(Predicate "goo nature")
			(ListLink (Concept "gar") (Concept "ah one")))
		(EvaluationLink
			(Predicate "goo nature")
			(ListLink (Concept "gar") (Concept "two") (Concept "three")))
	)
)

;; -------------------------------------------------------------
;; Assorted glob tests

(define glob-simple
	(MapLink
		(EvaluationLink (Predicate "goo")
			(ListLink (Concept "bar") (Glob "$x") (Concept "bif")))
		(SetLink
			(EvaluationLink (Predicate "foo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "gif")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "gar") (Concept "baz") (Concept "bif")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz")))
		)))

(define glob-simple-expected
	(SetLink
		(ListLink (Concept "baz"))
	))

(define glob-simple-tail
	(MapLink
		(EvaluationLink (Predicate "goo")
			(ListLink (Concept "bar") (Glob "$x")))
		(SetLink
			(EvaluationLink (Predicate "foo")
				(ListLink (Concept "bar") (Concept "baz")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "gif")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "gar") (Concept "baz")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz")))
		)))

(define glob-simple-tail-expected
	(SetLink
		(ListLink (Concept "baz"))
		(ListLink (Concept "baz") (Concept "gif"))
		(ListLink (Concept "baz") (Concept "bif"))
	))

(define glob-double
	(MapLink
		(EvaluationLink (Predicate "goo")
			(ListLink (Concept "bar") (Glob "$x") (Concept "bif") (Glob "$x")))
		(SetLink
			(EvaluationLink (Predicate "foo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif") (Concept "baz")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif") (Concept "baz")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "gif") (Concept "baz")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "gar") (Concept "baz") (Concept "bif") (Concept "baz")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "baz")))
		)))

(define glob-double-expected
	(SetLink
		(ListLink (Concept "baz"))
	))

(define glob-glob
	(MapLink
		(EvaluationLink (Predicate "goo")
			(ListLink (Concept "bar") (Glob "$x") (Concept "bif") (Glob "$x")))
		(SetLink
			(EvaluationLink (Predicate "foo")
				(ListLink (Concept "bar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "bif")
					(Concept "baz") (Concept "ni") (Concept "goh")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "bif")
					(Concept "baz") (Concept "ni") (Concept "goh")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "gar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "bif")
					(Concept "baz") (Concept "ni") (Concept "goh")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "gif")
					(Concept "baz") (Concept "ni") (Concept "goh")))
			(EvaluationLink (Predicate "goo")
				(ListLink (Concept "bar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "bif")
					(Concept "baz") (Concept "goh") (Concept "goh")))
		)))

(define glob-glob-expected
	(SetLink
		(ListLink
			(Concept "baz") (Concept "ni") (Concept "goh"))
	))
