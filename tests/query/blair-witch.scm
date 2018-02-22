;; Data set for testing issue
;;
;; https://github.com/opencog/atomspace/issues/1528
;;
;; blair-witch is here to emphasize the weirdness of the bug.

;; Query
(define query
(BindLink
  (AndLink
    (VariableNode "$f-lamb-e84bdd8")
    (NumberNode "2.000000")
    (EvaluationLink
      (GroundedPredicateNode "scm: dummy")
      (NumberNode "2.000000")
    )
  )
  (Concept "A"))
)

;; Grounded Predicate
(define (dummy X) (stv 1 1))
