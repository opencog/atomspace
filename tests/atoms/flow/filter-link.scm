;
; filter-link.scm -- Data for running the FilterLinkUTest
;
(use-modules (opencog) (opencog exec))

; ----------------------------------------------------------

(define single
	(FilterLink
		(LambdaLink
			(Variable "$x")
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(EdgeLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Concept "baz"))))
)

(define single-set
	(FilterLink
		(LambdaLink
			(Variable "$x")
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(SetLink
			; Not in alphaebtical or type-order!
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
		))
)

; Like above but does filtering.
(define single-set-filter
	(FilterLink
		(LambdaLink
			(Variable "$x")
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(SetLink
			; Not in alphaebtical or type-order!
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
			(EdgeLink
				(Predicate "oof dah")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
		))
)

; Same as above, but implicit scoping
(define single-set-noscope
	(FilterLink
		(EdgeLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Variable "$x")))
		(SetLink
			; Not in alphaebtical or type-order!
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
		))
)

; Like above but does implicit scope and filtering.
(define single-set-filter-noscope
	(FilterLink
		(EdgeLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Variable "$x")))
		(SetLink
			; Not in alphaebtical or type-order!
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
			(EdgeLink
				(Predicate "oof dah")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
		))
)

(define single-list
	(FilterLink
		(LambdaLink
			(Variable "$x")
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(ListLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
		))
)

(define single-type
	(FilterLink
		(LambdaLink
			(TypedVariable (Variable "$x") (Type "ConceptNode"))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(SetLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
		))
)

; ----------------------------------------------------------

(define single-signature
	(FilterLink
		(LambdaLink
			(TypedVariable (Variable "$x")
				(SignatureLink
					(EdgeLink
						(Predicate "foo")
						(ListLink (Concept "bar") (Type "ConceptNode")))))
			(Variable "$x"))
		(SetLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
		))
)

(define sig-expect
	(SetLink
		(EdgeLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Concept "ah one")))
		(EdgeLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Concept "ah two")))
	)
)

; ----------------------------------------------------------

(define double-num-set
	(FilterLink
		(LambdaLink
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "NumberNode")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y"))))
		(SetLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
		))
)

; ----------------------------------------------------------

(define double-con-set
	(FilterLink
		(LambdaLink
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "ConceptNode")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y"))))
		(SetLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
		))
)

;; =============================================================
;; RuleLink tests.

(define imply-map
	(FilterLink
		(RuleLink
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "ConceptNode")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y")))
			(EdgeLink
				(Predicate "reverse-foo")
				(ListLink (Variable "$y") (Variable "$x"))))
		(SetLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Number 3)))
		))
)

(define imply-expected
	(SetLink
		(EdgeLink
			(PredicateNode "reverse-foo")
			(ListLink (ConceptNode "ah one") (ConceptNode "bar")))
		(EdgeLink
			(PredicateNode "reverse-foo")
			(ListLink (ConceptNode "ah two") (ConceptNode "bar")))
	)
)

; ----------------------------------------------------------

(define imply-eval
	(FilterLink
		(RuleLink
			(VariableList
				(TypedVariable (Variable "$x") (Type "ConceptNode"))
				(TypedVariable (Variable "$y") (Type "NumberNode")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y")))
			(EdgeLink
				(Predicate "reverse-foo nature")
				(ListLink (Variable "$y") (Variable "$x"))))
		(SetLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Plus (Number 3) (Number 2))))
		))
)

(define eval-expected
	(SetLink
		(EdgeLink
			(PredicateNode "reverse-foo nature")
			(ListLink (Number 5) (ConceptNode "bar")))
	)
)

;; -------------------------------------------------------------
;; Implicit-variable RuleLink tests.

(define imply-map-nodecl
	(FilterLink
		(RuleLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Variable "$x") (Variable "$y")))
			(EdgeLink
				(Predicate "reverse-foo")
				(ListLink (Variable "$y") (Variable "$x"))))
		(SetLink
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "ah two")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Concept "too") (Concept "much")))
		))
)

; Above expects "imply-expected"

; ----------------------------------------------------------

(define imply-glob-nodecl
	(FilterLink
		(RuleLink
			(EdgeLink
				(Predicate "goo")
				(ListLink (Concept "bar") (Glob "$y")))
			(EdgeLink
				(Predicate "goo nature")
				(ListLink (Concept "gar") (Glob "$y"))))
		(SetLink
			(EdgeLink
				(Predicate "goo")
				(ListLink (Concept "bar")))
			(EdgeLink
				(Predicate "goo")
				(ListLink (Concept "bar") (Concept "ah one")))
			(EdgeLink
				(Predicate "goo")
				(ListLink (Concept "bar") (Concept "two") (Concept "three")))
			(EdgeLink
				(Predicate "foo")
				(ListLink (Concept "bar")))
		))
)

(define imply-glob-expected
	(SetLink
		(EdgeLink
			(Predicate "goo nature")
			(ListLink (Concept "gar") (Concept "ah one")))
		(EdgeLink
			(Predicate "goo nature")
			(ListLink (Concept "gar") (Concept "two") (Concept "three")))
	)
)

;; -------------------------------------------------------------
;; Assorted glob tests

(define glob-simple
	(FilterLink
		(EdgeLink (Predicate "goo")
			(ListLink (Concept "bar") (Glob "$x") (Concept "bif")))
		(SetLink
			(EdgeLink (Predicate "foo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "gif")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "gar") (Concept "baz") (Concept "bif")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz")))
		)))

(define glob-simple-expected
	(SetLink
		(ListLink (Concept "baz"))
	))

; ----------------------------------------------------------

(define glob-simple-tail
	(FilterLink
		(EdgeLink (Predicate "goo")
			(ListLink (Concept "bar") (Glob "$x")))
		(SetLink
			(EdgeLink (Predicate "foo")
				(ListLink (Concept "bar") (Concept "baz")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "gif")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "gar") (Concept "baz")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz")))
		)))

(define glob-simple-tail-expected
	(SetLink
		(ListLink (Concept "baz"))
		(ListLink (Concept "baz") (Concept "gif"))
		(ListLink (Concept "baz") (Concept "bif"))
	))

; ----------------------------------------------------------

(define glob-double
	(FilterLink
		(EdgeLink (Predicate "goo")
			(ListLink (Concept "bar") (Glob "$x") (Concept "bif") (Glob "$x")))
		(SetLink
			(EdgeLink (Predicate "foo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif") (Concept "baz")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "bif") (Concept "baz")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "gif") (Concept "baz")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "gar") (Concept "baz") (Concept "bif") (Concept "baz")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar") (Concept "baz") (Concept "baz")))
		)))

(define glob-double-expected
	(SetLink
		(ListLink (Concept "baz"))
	))

; ----------------------------------------------------------

(define glob-glob
	(FilterLink
		(EdgeLink (Predicate "goo")
			(ListLink (Concept "bar") (Glob "$x") (Concept "bif") (Glob "$x")))
		(SetLink
			(EdgeLink (Predicate "foo")
				(ListLink (Concept "bar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "bif")
					(Concept "baz") (Concept "ni") (Concept "goh")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "bif")
					(Concept "baz") (Concept "ni") (Concept "goh")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "gar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "bif")
					(Concept "baz") (Concept "ni") (Concept "goh")))
			(EdgeLink (Predicate "goo")
				(ListLink (Concept "bar")
					(Concept "baz") (Concept "ni") (Concept "goh")
					(Concept "gif")
					(Concept "baz") (Concept "ni") (Concept "goh")))
			(EdgeLink (Predicate "goo")
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

; ----------------------------------------------------------

(define local-quote-map
(FilterLink
  (LambdaLink
    (VariableList
      (VariableNode "$X")
      (VariableNode "$Y"))
    (LocalQuoteLink
      (AndLink
        (VariableNode "$X")
        (VariableNode "$Y"))))
  (AndLink
    (ConceptNode "A")
    (ConceptNode "B")))
)

; (cog-execute! local-quote-map)

;; The order of A and B depends on the canonical, though arbitrary,
;; order used to store outgoings in unordered links, consider both.
(define local-quote-map-result-1
  (ListLink
    (ConceptNode "A") (ConceptNode "B")))
(define local-quote-map-result-2
  (ListLink
    (ConceptNode "B") (ConceptNode "A")))

; ----------------------------------------------------------

(define quote-arg-map
(FilterLink
  (LambdaLink
    (VariableList
      (VariableNode "$X")
      (VariableNode "$Y"))
    (MemberLink
      (VariableNode "$X")
      (VariableNode "$Y")))
  (QuoteLink
    (MemberLink
      (DefinedSchemaNode "specialization-rule")
      (ConceptNode "pm-rbs"))))
)
; (cog-execute! quote-arg-map)

(define quote-arg-map-result
(ListLink
  (DefinedSchemaNode "specialization-rule")
  (ConceptNode "pm-rbs"))
)

; ----------------------------------------------------------

(define dont-exec-get
  (Filter
    (Rule
      (TypedVariable (Variable "x") (Type "ConceptNode"))
      (Variable "x")
      (DontExec (CollectionOf (Meet (And
        (Evaluation (GroundedPredicate "scm:filter") (Variable "x")))))))
    (Set (Concept "a") (Concept "b"))
  ))

; (cog-execute! dont-exec)

; (Variable "x") is bound in the CollectionOf/Meet; it cannot be substituted.
(define dont-expected
  (Set
    (CollectionOf (Meet
      (And
        (Evaluation
          (GroundedPredicate "scm:filter")
          (Variable "x")))))
    (CollectionOf (Meet
      (And
        (Evaluation
          (GroundedPredicate "scm:filter")
          (Variable "x")))))))

(define quote-get
  (Filter
    (Rule
      (TypedVariable (Variable "x") (Type "ConceptNode"))
      (Variable "x")
      (Quote (CollectionOf (Meet (Unquote (And
        (Evaluation (GroundedPredicate "scm:filter") (Variable "x"))))))))
    (Set (Concept "a") (Concept "b"))
  ))

; (cog-execute! quote-exec)

; The (Variable "x") is free, because the quote unbound it.
(define quote-expected
  (Set
    (CollectionOf (Meet
      (And
        (Evaluation
          (GroundedPredicate "scm:filter")
          (Concept "a")))))
    (CollectionOf (Meet
      (And
        (Evaluation
          (GroundedPredicate "scm:filter")
          (Concept "b")))))))


; ----------------------------------------------------------

(define siggy-filter
   (Filter

   (Signature
      (Edge
         (PredicateNode "foo")
         (List (Type 'Concept) (Type 'Concept))))
  (Set
     (Edge
        (Predicate "foo")
        (List (Concept "A") (Concept "B")))
     (Edge
        (PredicateNode "bar")
        (List (Concept "C") (Concept "D")))
     (Edge
        (Predicate "foo")
        (List (Number 5) (Number 6))))))

(define siggy-expect
  (Set
     (Edge
        (Predicate "foo")
        (List (Concept "A") (Concept "B")))))

; ----------------------------------------------------------

(DefineLink
   (DefinedSchema "foof-match")
   (Lambda
      (VariableList
         (TypedVariable (Variable "$x") (Type 'ConceptNode))
         (TypedVariable (Variable "$y") (Type 'ConceptNode)))
      (Edge
         (PredicateNode "foo")
         (List (Variable "$x") (Variable "$y")))))

(define defschema
   (Filter
      (DefinedSchema "foof-match")
      (Set
         (Edge
            (Predicate "foo")
            (List (Concept "A") (Concept "B")))
         (Edge
            (PredicateNode "bar")
            (List (Concept "C") (Concept "D")))
         (Edge
            (Predicate "foo")
            (List (Number 5) (Number 6))))))

(define defschema-expect
  (Set (List (Concept "A") (Concept "B"))))

; ----------------------------------------------------------

(DefineLink
   (DefinedSchema "foo to frob")
   (Rule
      (VariableList
         (TypedVariable (Variable "$x") (Type 'ConceptNode))
         (TypedVariable (Variable "$y") (Type 'ConceptNode)))
      (Edge
         (PredicateNode "foo")
         (List (Variable "$x") (Variable "$y")))
      (Member (Variable "$x") (Concept "all things frob"))
      (Member (Variable "$y") (Concept "orange-colored things"))))

(define frob-rule
   (Filter
      (DefinedSchema "foo to frob")
      (Set
         (Edge
            (Predicate "foo")
            (List (Concept "A") (Concept "B")))
         (Edge
            (PredicateNode "bar")
            (List (Concept "C") (Concept "D")))
         (Edge
            (Predicate "foo")
            (List (Number 5) (Number 6))))))

(define frob-expect
  (Set
    (List
      (Member
        (Concept "A")
        (Concept "all things frob"))
      (Member
        (Concept "B")
        (Concept "orange-colored things")))))

; ----------------------------------------------------------

(DefineLink
   (DefinedSchema "extract bc")
   (Rule
      (Variable "$x")
      (Edge
         ; (PredicateNode "foo")
         (Sign 'PredicateNode)
         (List (Sign 'Concept) (Variable "$x")))
      (Member (Variable "$x") (Concept "extract things"))))

(define sign-filter
   (Filter
      (DefinedSchema "extract bc")
      (Set
         (Edge
            (Predicate "foo")
            (List (Concept "A") (Concept "B")))
         (Edge
            (PredicateNode "bar")
            (List (Concept "C") (Concept "D")))
         (Edge
            (Predicate "foo")
            (List (Number 5) (Number 6))))))

(define sign-expect
(SetLink
  (MemberLink
    (ConceptNode "D")
    (ConceptNode "extract things"))
  (MemberLink
    (ConceptNode "B")
    (ConceptNode "extract things"))))

; ----------------------------------------------------------
