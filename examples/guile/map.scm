;
; Mapping example.
;
; The MapLink implements a link type analogous to the `map` function
; commmonly found in functional programming langauges, such as the
; scheme srfi-1 `map`, or `map` in haskell.
;
; In many ways, MapLink is similar to BindLink, except that MapLink
; does not search the entire atomspace for matching patterns; rather,
; it only examines the given input list/set, and applies the map to
; that.
;
; In many ways, MapLink is the opposite of PutLink, in that it un-does
; a beta reduction, by extracting values from a matching pattern. Thus,
; MapLink could have been named UnPutLink, CoPutLink or ExtractLink or
; UnBetaReduceLink.
;
; These ideas are illustrated below.

(use-modules (opencog) (opencog exec))

(define single
	(MapLink
		; Define a pattern that will be used to extract a value
		; from a pattern. The extracted value will correspond to
		; the place-holder variable $x.
		(ScopeLink
			(Variable "$x")
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		; The graph from which a value is to be extracted.  Clearly,
		; the variable $x corresponds to Concept "baz"
		(EvaluationLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Concept "baz"))))
)

; This should return (Concept "baz") as that is the extracted value
; for the variable $x.
(cog-execute! single)

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

(define imply-map
	(MapLink
		(ExtensionalImplicationLink
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
		(ExtensionalImplicationLink
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
