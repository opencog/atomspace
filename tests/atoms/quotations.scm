(define vardecl
(VariableList
  (TypedVariableLink
    (VariableNode "$x-0-47189056")
    (TypeNode "VariableNode")
  )
  (TypedVariableLink
    (VariableNode "$x-1-518dbc2b")
    (TypeNode "VariableNode")
  )
  (VariableNode "$g-body-2653368")
)
)

(define ill-quoted
  (QuoteLink
    (LambdaLink
      (UnquoteLink
        (VariableList
          (VariableNode "$x-0-47189056")
          (VariableNode "$x-1-518dbc2b")
        )
      )
      (UnquoteLink
        (VariableNode "$g-body-2653368")
      )
    )
  )
)

(define well-quoted
  (QuoteLink
    (LambdaLink
      (UnquoteLink
        (VariableList
          (VariableNode "$x-0-47189056")
          (VariableNode "$x-1-518dbc2b")
        )
      )
      (UnquoteLink
        (VariableNode "$g-body-2653368")
      )
    )
  )
)

(define quoted-lambda
  (QuoteLink
    (LambdaLink
      (UnquoteLink
      (VariableNode "$vardecl"))
    (UnquoteLink
      (VariableNode "$body")))))

(define quoted-clauses
(LocalQuoteLink
  (AndLink
    (QuoteLink
      (LambdaLink
        (UnquoteLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
        )
        (UnquoteLink
          (EvaluationLink
            (PredicateNode "contain")
            (ListLink
              (ConceptNode "treatment-1")
              (ConceptNode "compound-A")
            )
          )
        )
      )
    )
    (QuoteLink
      (LambdaLink
        (UnquoteLink
          (TypedVariableLink
            (VariableNode "$X")
            (TypeNode "ConceptNode")
          )
        )
        (UnquoteLink
          (EvaluationLink
            (PredicateNode "take")
            (ListLink
              (VariableNode "$X")
              (ConceptNode "treatment-1")
            )
          )
        )
      )
    )
  )
)
)

(define consumed-quoted-clauses
(LocalQuoteLink
  (AndLink
    (LambdaLink
      (TypedVariableLink
        (VariableNode "$X")
        (TypeNode "ConceptNode")
      )
      (EvaluationLink
        (PredicateNode "contain")
        (ListLink
          (ConceptNode "treatment-1")
          (ConceptNode "compound-A")
        )
      )
    )
    (LambdaLink
      (TypedVariableLink
        (VariableNode "$X")
        (TypeNode "ConceptNode")
      )
      (EvaluationLink
        (PredicateNode "take")
        (ListLink
          (VariableNode "$X")
          (ConceptNode "treatment-1")
        )
      )
    )
  )
)
)

(define quoted-grounded-predicate-argument
  (EvaluationLink
    (GroundedPredicateNode "scm: absolutely-true")
    (EvaluationLink
      (PredicateNode "minsup")
      (ListLink
        (QuoteLink
          (LambdaLink
            (UnquoteLink
              (VariableNode "$X")
            )
            (Unquote
              (VariableNode "$X")
            )
          )
        )
        (ConceptNode "texts")
        (NumberNode "5.000000")
      )
    )
  )
)

(define consumed-quoted-grounded-predicate-argument
  (EvaluationLink
    (GroundedPredicateNode "scm: absolutely-true")
    (EvaluationLink
      (PredicateNode "minsup")
      (ListLink
        (LambdaLink
          (VariableNode "$X")
          (VariableNode "$X")
        )
        (ConceptNode "texts")
        (NumberNode "5.000000")
      )
    )
  )
)

(define quoted-not-X
  (LocalQuoteLink
    (NotLink
      (VariableNode "$X")
    )
  )
)
