;see comment section in EvalLinkDefaultTVUTest.cxxtest 
;see the discurssion at https://github.com/opencog/atomspace/issues/1868

(define (check-color object color)
    (if (string=? (cog-name object) "RedItem")
        (stv 0.55 0.55)
        (stv 0.45 0.45))
)

(define red-thing (ConceptNode "RedItem"))
(define tr-thing (ConceptNode "TransparentItem"))

(Inheritance (stv 1.0 0.999) red-thing (ConceptNode "colored"))
(Inheritance (stv 1.0 0.999) tr-thing (ConceptNode "colored"))
(Inheritance (stv 1.0 0.999) (ConceptNode "Red") (ConceptNode "Color"))

(define has-color
        (EvaluationLink
            (GroundedPredicateNode "scm:check-color")
            (ListLink (VariableNode "$X")
                      (VariableNode "$C"))
        )
)


(define query
    (BindLink
        (AndLink
          (InheritanceLink (VariableNode "$X") (ConceptNode "colored"))
          (InheritanceLink (VariableNode "$C") (ConceptNode "Color"))
          has-color
        )
    has-color)
)
