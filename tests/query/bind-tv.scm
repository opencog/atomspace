(Inheritance (stv 0.99 0.99) (Concept "BB1") (Concept "BoundingBox"))

(define (redness box ) 
    (stv 0.55 0.55)
)

(define eval1 (EvaluationLink
    (GroundedPredicateNode  "scm: redness")
    (Variable "$X"))
)


(define find-red 
    (BindLink (VariableNode "$X")
              (AndLink eval1
                       (InheritanceLink (VariableNode "$X") (ConceptNode "BoundingBox")))
              eval1
    )
)

