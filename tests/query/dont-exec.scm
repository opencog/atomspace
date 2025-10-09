(define simple-bl
  (QueryLink
    (AndLink)
    (Concept "Hello World")))

(EvaluationLink
  (PredicateNode "URE:BC:and-BIT")
  simple-bl)

(define bl
  (CollectionOf
  (QueryLink
    (TypedVariableLink
      (VariableNode "$A")
      (TypeNode "QueryLink")
    )
    (EvaluationLink
      (PredicateNode "URE:BC:and-BIT")
      (VariableNode "$A")
    )
    (DontExecLink
      (VariableNode "$A")
    )
  ))
)
