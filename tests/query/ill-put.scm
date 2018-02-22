(use-modules (opencog logger))

;; Well formed PutLink
(define put-1
(PutLink
  (LambdaLink
    (VariableNode "$X")
    (VariableNode "$X"))
  (ConceptNode "A"))
)

;; Ill formed PutLinks (only detectable at run-time)
(define put-2
(PutLink
  put-1
  (Concept "B"))
)
(define put-3
(PutLink
  put-2
  (Concept "C"))
)

(define get-put
(Get
  (TypedVariable
    (Variable "$P")
    (Type "PutLink"))
  (And
    (Variable "$P")
    (Evaluation
      (GroundedPredicate "scm: well-formed?")
      (Variable "$P"))))
)

(define (well-formed? P)
  (cog-logger-debug "well-formed? P = ~a" P)
  (stv 1 1))
