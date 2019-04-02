;; To express that all balls are green based on a collection of facts
;; we introduce a predicate
;;
;; Predicate "based-on-evidence"
;;
;; Which we use as follows
;;
;; Evaluation (stv 1 3/800)
;;   Predicate "based-on-evidence"
;;   List
;;     ImplicationScope
;;       TypedVariable
;;         X
;;         Type "ConceptNode"
;;       Inheritance
;;         X
;;         ball
;;       Inheritance
;;         X
;;         green
;;     Set
;;       List
;;         Inheritance B1 ball
;;         Inheritance B1 green
;;       List
;;         Inheritance B2 ball
;;         Inheritance B2 green
;;       List
;;         Inheritance B3 ball
;;         Inheritance B3 green

;; Variable
(define X (Variable "$X"))
(define E (Variable "$E"))
(define G (Glob "$G"))

;; Balls
(define B1 (Concept "B1"))
(define B2 (Concept "B2"))
(define B3 (Concept "B3"))

;; Classes
(define ball (Concept "ball"))
(define green (Concept "green"))

;; Define target with all evidence already stated
(define target-known-evidence
  (Evaluation
    (Predicate "based-on-evidence")
    (List
      ;; Implication scope
      (ImplicationScope
        (TypedVariable
          X
          (Type "ConceptNode"))
        (Inheritance X ball)
        (Inheritance X green))
      ;; Evidence
      (Set
        (List
          (Inheritance B1 ball)
          (Inheritance B1 green))
        (List
          (Inheritance B2 ball)
          (Inheritance B2 green))
        (List
          (Inheritance B3 ball)
          (Inheritance B3 green))))))

;; Define target with all evidence unknown
(define target-unknown-evidence
  (Evaluation
    (Predicate "based-on-evidence")
    (List
      ;; Implication scope
      (ImplicationScope
        (TypedVariable
          X
          (Type "ConceptNode"))
        (Inheritance X ball)
        (Inheritance X green))
      ;; Evidence
      E)))

(define vardecl-unknown-evidence
  (TypedVariable
    E
    (Type "SetLink")))

;; Define target with all evidence unknown, using Glob
(define target-unknown-evidence-with-glob
  (Evaluation
    (Predicate "based-on-evidence")
    (List
      ;; Implication scope
      (ImplicationScope
        (TypedVariable
          X
          (Type "ConceptNode"))
        (Inheritance X ball)
        (Inheritance X green))
      ;; Evidence
      (Set G))))

;; TODO: the type of G should be further specified, such the number of
;; elements, or possible range, as well as possible the type of each
;; element, e.g. List, once the backward chainer supports deep types.
