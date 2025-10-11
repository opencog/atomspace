;; KB

(EvaluationLink
  (PredicateNode "P")
  (LambdaLink
    (ImplicationLink
      (VariableNode "$Z")
      (InheritanceLink
        (VariableNode "$X")
        (VariableNode "$Y")
      )
    )
  )
)

(EvaluationLink
  (PredicateNode "P")
  (LambdaLink
    (ImplicationLink
      (VariableNode "$X")
      (VariableNode "$Y")
    )
  )
)

;; Query

(define (dummy x) #t)

(define query
  (CollectionOf (Query
    (AndLink
      (EvaluationLink
        (GroundedPredicateNode "scm: dummy")
        (EvaluationLink
          (PredicateNode "P")
          (LambdaLink
            (ImplicationLink
              (InheritanceLink
                (VariableNode "$X")
                (VariableNode "$Y")
              )
              (VariableNode "$Z")
            )
          )
        )
      )
      (PresentLink
        (EvaluationLink
          (PredicateNode "P")
          (LambdaLink
            (ImplicationLink
              (InheritanceLink
                (VariableNode "$X")
                (VariableNode "$Y")
              )
              (VariableNode "$Z")
            )
          )
        )
      )
    )
    (Concept "OK")
  ))
)
