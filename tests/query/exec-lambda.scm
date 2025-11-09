(define (dummy x) x)

(define bl
   (Query
      (VariableList
         (TypedVariable (Variable "$V") (Type 'Variable))
         (TypedVariable (Variable "$B") (Type 'Edge)))
      (And
         (Variable "$V")
         (Variable "$B"))
      (ExecutionOutput
         (GroundedSchema "scm: dummy")
         (ListLink
             (Lambda
                 (Variable "$V")
                 (Variable "$B"))))
   ))

(Edge (Predicate "P") (Concept "A"))
