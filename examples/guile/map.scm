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
		; The same pattern as above.  Extracts values for variable $x.
		(ScopeLink
			(Variable "$x")
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(SetLink
			; A set of graphs to which the above pattern should
			; be matched.
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

; This should return
; (SetLink (Concept "ah one") (Concept "ah two") (Number 3))
; since these are the three values that correspond to the variable $x.
; Note that SetLinks are unordered links, and so the members of the
; set may be returned in a different order than in which they were
; specified as inputs.
(cog-execute! single-set)


; This example is like the above, except that a ListLink instead of
; a SetLink is used. The ListLink is an ordered link; the ordering
; of the list is preserved by the mapping.
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
				(ListLink (Concept "bar") (Number 3)))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
		))
)

; This should return
; (ListLink (Concept "ah one") (Number 3) (Concept "ah two"))
; Note that the sequential order of the original list is preserved.
(cog-execute! single-list)


; Same as above, except that a type constraint is applied.
; This causes the input to be filtered, so that any graphs
; that don't match the type are discarded.
(define single-type
	(MapLink
		(ScopeLink
			; The type ov the variable MUST be ConceptNode!!
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(SetLink
			; A set of graphs to which the above pattern should be
			; applied.  Note that, due to the type constraint, only
			; two of the three can match. (Numbers not being Concepts)
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

; The expected return value is
; (SetLink (ConceptNode "ah one") (ConceptNode "ah two"))
; Note that the NumberNode is no longer included in the result
; of the map.
(cog-execute! single-type)


; The type checking that can be performed during mapping can be
; quite sophisticated. In this example, a single variable $x is
; used to match *every* element in the input set.  However, since
; the variable $x is typed to have a very specific form, some of
; the input set is rejected, as it does not match that type.
;
; This implements a kind of filtering, by returning a subset of
; the original input set, and discarding those values that don't
; match the desired type.  A similar kind of filtering can be done
; using PutLink; see the `filter.scm` example for more.
;
(define single-signature
	(MapLink
		(ScopeLink
         ; The variable $x must be an evaluationLink of a certain form!
			(TypedVariable (Variable "$x")
				(SignatureLink
					(EvaluationLink
						(Predicate "foo")
						(ListLink (Concept "bar") (Type "ConceptNode")))))
			(Variable "$x"))
		(SetLink
			; Of the three elements in this set, only two have the type
			; that is specified above.
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

; Executing this will filter the input set down to just two members.
; The return value should be this:
;    (SetLink
;       (EvaluationLink
;          (Predicate "foo")
;          (ListLink (Concept "bar") (Concept "ah one")))
;       (EvaluationLink
;          (Predicate "foo")
;          (ListLink (Concept "bar") (Concept "ah two")))
;    )
;
(cog-execute! single-signature)

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
