(define (dummy x) x)

(define bl
   (BindLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$V")
            (TypeNode "VariableNode")
         )
         (TypedVariableLink
            (VariableNode "$B")
            (TypeNode "EvaluationLink")
         )
      )
      (AndLink
         (VariableNode "$V")
         (VariableNode "$B")
      )
      (ExecutionOutputLink
         (GroundedSchemaNode "scm: dummy")
         (ListLink
            (QuoteLink
               (LambdaLink
                  (UnquoteLink
                     (VariableNode "$V")
                  )
                  (UnquoteLink
                     (VariableNode "$B")
                  )
               )
            )
         )
      )
   )
)

(Evaluation (Predicate "P") (Concept "A"))
