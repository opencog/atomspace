;; Alternative way of formalizing the frog example. Here the
;; implications are replaced by rules to form the rule base.

;;;;;;;;;;;;;;;;;;;;
;; Knowledge base ;;
;;;;;;;;;;;;;;;;;;;;

(Evaluation (stv 1.0 1.0)
   (Predicate "croaks")
   (Concept "Fritz"))

(Evaluation (stv 1.0 1.0)
   (Predicate "chirps")
   (Concept "Tweety"))

(Inheritance (stv 1.0 1.0)
   (Concept "Tweety")
   (Concept "yellow"))

(Evaluation (stv 1.0 1.0)
   (Predicate "eats_flies")
   (Concept "Tweety"))

(Evaluation (stv 1.0 1.0)
   (Predicate "eats_flies")
   (Concept "Fritz"))

;;;;;;;;;;;;;;;
;; Rule base ;;
;;;;;;;;;;;;;;;

;; In this example the implication relationships are directly
;; represented as rules.

(define if-croaks-and-eats-flies-then-frog-rule
  (BindLink
    (Variable "$X")
    (And
      (Evaluation
        (Predicate "croaks")
        (Variable "$X")
      )
      (Evaluation
        (Predicate "eats_flies")
        (Variable "$X")
      )
    )
    (Inheritance
      (Variable "$X")
      (Concept "frog")
    )
  )
)

(define if-croaks-and-eats-flies-then-frog-rule-name
  (DefinedSchema "if-croaks-and-eats-flies-then-frog-rule"))
(Define if-croaks-and-eats-flies-then-frog-rule-name
  if-croaks-and-eats-flies-then-frog-rule)

(define if-frog-then-green-rule
  (Bind
    (Variable "$X")
    (Inheritance
      (Variable "$X")
      (Concept "frog")
    )
    (Inheritance
      (Variable "$X")
      (Concept "green")
    )
  )
)

(define if-frog-then-green-rule-name
  (DefinedSchema "if-frog-then-green-rule"))
(Define if-frog-then-green-rule-name
  if-frog-then-green-rule)

(define frog-rb (Concept "frog-rb"))

(Inheritance
  frog-rb
  (Concept "URE")
)

(Execution
  (Schema "URE:maximum-iterations")
  frog-rb
  (Number 20)
)

(Member (stv 0.9 1)
  if-croaks-and-eats-flies-then-frog-rule-name
  frog-rb
)

(Member (stv 0.5 1)
  if-frog-then-green-rule-name
  frog-rb
)
