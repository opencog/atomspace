(define (dummy x) x)

(define bl
   (QueryLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$V")
            (TypeNode "VariableNode")
         )
         (TypedVariableLink
            (VariableNode "$B")
            (TypeNode "EdgeLink")
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

(Edge (Predicate "P") (Concept "A"))
