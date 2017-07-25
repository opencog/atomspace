(define simple-bl
  (BindLink
    (AndLink)
    (Concept "Hello World")))

(EvaluationLink (stv 1 1)
  (PredicateNode "URE:BC:and-BIT")
  simple-bl)

(define bl
  (BindLink
    (TypedVariableLink
      (VariableNode "$A")
      (TypeNode "BindLink")
    )
    (EvaluationLink
      (PredicateNode "URE:BC:and-BIT")
      (VariableNode "$A")
    )
    (DontExecLink
      (VariableNode "$A")
    )
  )
)
