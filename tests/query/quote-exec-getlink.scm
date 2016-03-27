(use-modules (opencog) (opencog exec))

(define (foo arg)
    (stv 1 1)
)

(define z-eval
    (EvaluationLink
        (GroundedPredicateNode "scm: foo")
        (ListLink (Node "argument"))))

(define z-def
    (DefineLink
         (DefinedPredicateNode "test")
         z-eval
    )
)

(define z-get
    (GetLink
       (TypedVariableLink
          (VariableNode "$effect")
          (TypeNode "ConceptNode")
       )
       (EvaluationLink
          (PredicateNode "some property of dpn")
          (ListLink
             (QuoteLink (DefinedPredicateNode "test"))
             (VariableNode "$effect")
          )
       )
    )
)

(EvaluationLink
   (PredicateNode "some property of dpn")
   (ListLink
      (DefinedPredicateNode "test")
      (ConceptNode "result")
   )
)
