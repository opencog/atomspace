;
; filter-link.scm -- Data for running the FilterLinkUTest
;
(use-modules (opencog) (opencog exec))

; ----------------------------------------------------------

(define single
	(FilterLink
		(LambdaLink
			(Variable "$x")
			(EvaluationLink
				(Predicate "foo")
				(ListLink (Concept "bar") (Variable "$x"))))
		(EvaluationLink
			(Predicate "foo")
			(ListLink (Concept "bar") (Concept "baz"))))
)

(define single-set
	(FilterLink
		(LambdaLink
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
	(FilterLink
		(LambdaLink
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
	(FilterLink
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
	(FilterLink
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
	(FilterLink
		(LambdaLink
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
	(FilterLink
		(LambdaLink
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

; ----------------------------------------------------------

(define single-signature
	(FilterLink
		(LambdaLink
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

; ----------------------------------------------------------

(define double-num-set
	(FilterLink
		(LambdaLink
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

; ----------------------------------------------------------

(define double-con-set
	(FilterLink
		(LambdaLink
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

;; =============================================================
;; RuleLink tests.

(define imply-map
	(FilterLink
		(RuleLink
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

; ----------------------------------------------------------

(define imply-eval
	(FilterLink
		(RuleLink
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
;; Implicit-variable RuleLink tests.

(define imply-map-nodecl
	(FilterLink
		(RuleLink
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

; ----------------------------------------------------------

(define imply-glob-nodecl
	(FilterLink
		(RuleLink
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
	(FilterLink
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

; ----------------------------------------------------------

(define glob-simple-tail
	(FilterLink
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

; ----------------------------------------------------------

(define glob-double
	(FilterLink
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

; ----------------------------------------------------------

(define glob-glob
	(FilterLink
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
      (DontExec (Get (And
        (Evaluation (GroundedPredicate "scm:filter") (Variable "x"))))))
    (Set (Concept "a") (Concept "b"))
  ))

; (cog-execute! dont-exec)

; (Variable "x") is bound in the Get; it cannot be substituted.
(define dont-expected
  (Set
    (Get
      (And
        (Evaluation
          (GroundedPredicate "scm:filter")
          (Variable "x"))))
    (Get
      (And
        (Evaluation
          (GroundedPredicate "scm:filter")
          (Variable "x"))))))

(define quote-get
  (Filter
    (Rule
      (TypedVariable (Variable "x") (Type "ConceptNode"))
      (Variable "x")
      (LocalQuote (Get (And
        (Evaluation (GroundedPredicate "scm:filter") (Variable "x"))))))
    (Set (Concept "a") (Concept "b"))
  ))

; (cog-execute! quote-exec)

; The (Variable "x") is free, because the quote unbound it.
(define quote-expected
  (Set
    (Get
      (And
        (Evaluation
          (GroundedPredicate "scm:filter")
          (Concept "a"))))
    (Get
      (And
        (Evaluation
          (GroundedPredicate "scm:filter")
          (Concept "b"))))))


; ----------------------------------------------------------

(define siggy-filter
   (Filter

   (Signature
      (Evaluation
         (PredicateNode "foo")
         (List (Type 'Concept) (Type 'Concept))))
  (Set
     (Evaluation
        (Predicate "foo")
        (List (Concept "A") (Concept "B")))
     (Evaluation
        (PredicateNode "bar")
        (List (Concept "C") (Concept "D")))
     (Evaluation
        (Predicate "foo")
        (List (Number 5) (Number 6))))))

(define siggy-expect
  (Set
     (Evaluation
        (Predicate "foo")
        (List (Concept "A") (Concept "B")))))

; ----------------------------------------------------------

(DefineLink
   (DefinedSchema "foof-match")
   (Lambda
      (VariableList
         (TypedVariable (Variable "$x") (Type 'ConceptNode))
         (TypedVariable (Variable "$y") (Type 'ConceptNode)))
      (Evaluation
         (PredicateNode "foo")
         (List (Variable "$x") (Variable "$y")))))

(define defschema
   (Filter
      (DefinedSchema "foof-match")
      (Set
         (Evaluation
            (Predicate "foo")
            (List (Concept "A") (Concept "B")))
         (Evaluation
            (PredicateNode "bar")
            (List (Concept "C") (Concept "D")))
         (Evaluation
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
      (Evaluation
         (PredicateNode "foo")
         (List (Variable "$x") (Variable "$y")))
      (Member (Variable "$x") (Concept "all things frob"))
      (Member (Variable "$y") (Concept "orange-colored things"))))

(define frob-rule
   (Filter
      (DefinedSchema "foo to frob")
      (Set
         (Evaluation
            (Predicate "foo")
            (List (Concept "A") (Concept "B")))
         (Evaluation
            (PredicateNode "bar")
            (List (Concept "C") (Concept "D")))
         (Evaluation
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
      (Evaluation
         ; (PredicateNode "foo")
         (Sign 'PredicateNode)
         (List (Sign 'Concept) (Variable "$x")))
      (Member (Variable "$x") (Concept "extract things"))))

(define sign-filter
   (Filter
      (DefinedSchema "extract bc")
      (Set
         (Evaluation
            (Predicate "foo")
            (List (Concept "A") (Concept "B")))
         (Evaluation
            (PredicateNode "bar")
            (List (Concept "C") (Concept "D")))
         (Evaluation
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
