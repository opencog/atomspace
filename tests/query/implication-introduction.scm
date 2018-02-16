(define and-lambda-distribution-rule
   (BindLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$TyVs")
            (TypeChoice
               (TypeNode "TypedVariableLink")
               (TypeNode "VariableNode")
               (TypeNode "VariableList")
            )
         )
         (TypedVariableLink
            (VariableNode "$And")
            (TypeNode "AndLink")
         )
      )
      (QuoteLink
         (LambdaLink
            (UnquoteLink
               (VariableNode "$TyVs")
            )
            (UnquoteLink
               (VariableNode "$And")
            )
         )
      )
      (ExecutionOutputLink
         (GroundedSchemaNode "scm: dummy")
         (ListLink
            (QuoteLink
               (LambdaLink
                  (UnquoteLink
                     (VariableNode "$TyVs")
                  )
                  (UnquoteLink
                     (VariableNode "$And")
                  )
               )
            )
         )
      )
   )
)

(define implication-introduction-rule
   (BindLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$P")
            (TypeChoice
               (TypeNode "PredicateNode")
               (TypeNode "LambdaLink")
            )
         )
         (TypedVariableLink
            (VariableNode "$Q")
            (TypeChoice
               (TypeNode "PredicateNode")
               (TypeNode "LambdaLink")
            )
         )
      )
      (AndLink
         (VariableNode "$P")
         (VariableNode "$Q")
         (NotLink
            (EqualLink
               (VariableNode "$P")
               (VariableNode "$Q")
            )
         )
      )
      (ExecutionOutputLink
         (GroundedSchemaNode "scm: dummy")
         (ListLink
            (ImplicationLink
               (VariableNode "$P")
               (VariableNode "$Q")
            )
            (VariableNode "$P")
            (VariableNode "$Q")
         )
      )
   )
)

(define (dummy . args) (Concept "dummy"))
