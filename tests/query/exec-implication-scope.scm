(define (dummy x) x)

(Evaluation
   (Predicate "P")
   (Concept "A"))

(define bl
(Bind
   (Present
      (Evaluation
         (Predicate "P")
         (Variable "$X")))
   (ExecutionOutput
      (GroundedSchema "scm: dummy")
      (ImplicationScope
         (Evaluation
            (Predicate "P")
            (Variable "$X"))
         (Evaluation
            (Predicate "Q")
            (Variable "$X"))))))
