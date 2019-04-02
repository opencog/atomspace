;; Evidence Based Conditional Direct Evaluation rule
;;
;; This is a form a conditional direct evaluation but keep track of
;; the evidence used to make the evaluation.
;;
;; Evaluation
;;   Predicate "based-on-evidence"
;;   List
;;     ImplicationScope
;;       X
;;       P[X]
;;       Q[X]
;;     Set ES
;; P[T]
;; Q[T]     // such that T is not in ES
;; |-
;; Evaluation
;;   Predicate "based-on-evidence"
;;   List
;;     ImplicationScope
;;       X
;;       P[X]
;;       Q[X]
;;     Set ES T
;;
;; where P[X] and Q[X] are some expression where X appears. ES is a
;; glob node representing the terms used to construct or retrieve the
;; evidence of the implication scope in the premise, and E is the new
;; term of evidence.

;; TODO: resume once GlobNode is supported

(define evidence-based-conditional-direct-evaluation-implication-scope-rule
  ;; (let* ((X (Variable "$X"))
  ;;        (PX (
  ;; (Bind
)

(define (evidence-based-conditional-direct-evaluation-implication-scope-formula
         conclusion . premises)
)
