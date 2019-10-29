(EvaluationLink
  (PredicateNode "minsup")
  (ListLink
    (LambdaLink
      (VariableNode "$spe-arg-1")
      (InheritanceLink
        (ConceptNode "A")
        (VariableNode "$spe-arg-1")
      )
    )
    (ConceptNode "texts")
    (NumberNode "2")
  )
)

(EvaluationLink
  (PredicateNode "minsup")
  (ListLink
    (LambdaLink
      (VariableNode "$spe-arg-0")
      (InheritanceLink
        (VariableNode "$spe-arg-0")
        (ConceptNode "C")
      )
    )
    (ConceptNode "texts")
    (NumberNode "2")
  )
)

(define query
(GetLink
  (TypedVariableLink
    (VariableNode "$f")
    (TypeNode "LambdaLink")
  )
  (AndLink
    (VariableNode "$f")
    (EvaluationLink
      (PredicateNode "minsup")
      (ListLink
        (LambdaLink
          (VariableNode "$top-arg")
          (VariableNode "$top-arg")
        )
        (ConceptNode "texts")
        (NumberNode "2")
      )
    )
  )
)
)

(define expected
(Set
  (LambdaLink
    (VariableNode "$spe-arg-1")
    (InheritanceLink
      (ConceptNode "A")
      (VariableNode "$spe-arg-1")
    )
  )
  (LambdaLink
    (VariableNode "$spe-arg-0")
    (InheritanceLink
      (VariableNode "$spe-arg-0")
      (ConceptNode "C")
    )
  )
  (LambdaLink
    (VariableNode "$top-arg")
    (VariableNode "$top-arg")
  )
)
)
