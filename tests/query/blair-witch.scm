;; Data set for testing issue
;;
;; https://github.com/opencog/atomspace/issues/1528
;;
;; blair-witch is here to emphasize the weirdness of the bug.

;; Grounded Predicate
(define (truth X) (cog-new-stv 1 1))
(define (alternative-fact X) (cog-new-stv 0 1))

;; Query
(define find-something
(CollectionOf
(QueryLink
  (AndLink
    (VariableNode "$f-lamb-e84bdd8")
    (NumberNode "2.000000")
    (EvaluationLink
      (GroundedPredicateNode "scm: truth")
      (NumberNode "2.000000")
    )
  )
  (Concept "A"))
)
)

(define find-nothing
(CollectionOf
(QueryLink
  (AndLink
    (VariableNode "$f-lamb-e84bdd8")
    (NumberNode "2.000000")
    (EvaluationLink
      (GroundedPredicateNode "scm: alternative-fact")
      (NumberNode "2.000000")
    )
  )
  (Concept "A"))
)
)
