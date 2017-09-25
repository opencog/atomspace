(define and-bit-prior
  (ImplicationScope (stv 0.0001 0.001)
    (VariableList
      (TypedVariable
        (Variable "$A")
        (Type "DontExecLink"))
      (Variable "$T"))
    (And
      (Evaluation
        (Predicate "URE:BC:and-BIT")
        (Variable "$A"))
      (Evaluation
        (Predicate "URE:BC:target")
        (Variable "$T")))
    (Evaluation
      (Predicate "URE:BC:preproof")
      (List
        (Variable "$A")
        (Variable "$T")))))

;; Load useful PLN rules
(load-from-path "tests/scm/conditional-full-instantiation.scm")

(define and-bit-prior-rule (car (cog-outgoing-set (cog-bind conditional-full-instantiation-implication-scope-meta-rule))))
