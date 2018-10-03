

(define (redness node)
    (stv 0.55 0.55)
)

(define red-thing (ConceptNode "RedItem"))

(Inheritance (stv 1.0 0.999) red-thing (ConceptNode "colored"))

(define is-red 
        (EvaluationLink
            (GroundedPredicateNode "scm:redness")
            (VariableNode "$X")
        )
)

(define query
    (BindLink
        (AndLink
           (InheritanceLink (VariableNode "$X") (ConceptNode "colored"))
           is-red
        )
    is-red)
) 
