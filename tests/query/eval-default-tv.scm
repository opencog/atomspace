;see comment section in EvalLinkDefaultTVUTest.cxxtest 
;see the discurssion at https://github.com/opencog/atomspace/issues/1868

(define (check-color object color)
    (string=? (cog-name object) "RedItem")
)

(define red-thing (ConceptNode "RedItem"))
(define tr-thing (ConceptNode "TransparentItem"))

(Inheritance red-thing (ConceptNode "colored"))
(Inheritance tr-thing (ConceptNode "colored"))
(Inheritance (ConceptNode "Red") (ConceptNode "Color"))

(define has-color
        (EvaluationLink
            (GroundedPredicateNode "scm:check-color")
            (ListLink (VariableNode "$X")
                      (VariableNode "$C"))
        )
)


(define query
    (CollectionOf
    (QueryLink
        (AndLink
          (InheritanceLink (VariableNode "$X") (ConceptNode "colored"))
          (InheritanceLink (VariableNode "$C") (ConceptNode "Color"))
          has-color
        )
    has-color)
    )
)
