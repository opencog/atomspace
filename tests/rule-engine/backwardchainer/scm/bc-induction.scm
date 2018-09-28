;; KB. Instances that seem to suggest that P implies Q

(Evaluation (stv 1 1)
   (Predicate "P")
   (Concept "A"))

(Evaluation (stv 1 1)
   (Predicate "P")
   (Concept "B"))

(Evaluation (stv 1 1)
   (Predicate "P")
   (Concept "C"))

(Evaluation (stv 1 1)
   (Predicate "Q")
   (Concept "A"))

(Evaluation (stv 1 1)
   (Predicate "Q")
   (Concept "B"))

;; Calculate the TV of that given that existing evidence. In order to
;; do that the Backward Chainer will have to
;;
;; 1. Back chain the meta-rule conditional instantiation from the target
;;
;; 2. Back chain the implication scope direct evaluation from the meta
;;    premise of the meta-rule conditional instantiation to generate
;;    implication based on the evidence.
(define bc-induction-target
  (Evaluation
     (Predicate "Q")
     (Concept "C")))
