;; Query
(define query
  (let* (
     ;; Constants
     (R (Predicate "R"))
     (A (Execution (Schema "A")))
     ;; Variables
     (P (Variable "$P"))
     (Q (Variable "$Q"))
     ;; Clauses
     (P→Q (Quote
            (Implication
              (Unquote P)
              (Unquote Q))))
     (Q∧A (And Q A))
     (Q∧A→R (Implication
              Q∧A
              R)))
    ;; Query
    (Get
      (VariableSet P Q)
      (Present P→Q Q∧A→R))))

;; KB

;; Culprit
(Implication
  (And
    (Predicate "Qbis") ; Culprit
    (Execution (Schema "A"))
  )
  (Predicate "R"))

;; Premises
(Implication
  (Predicate "P")
  (Predicate "Q"))
(Implication
  (And
    (Predicate "Q")
    (Execution (Schema "A"))
  )
  (Predicate "R"))
