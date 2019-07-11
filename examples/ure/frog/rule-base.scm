;;
;; Configuration file for the frog example
;;
;; To be loaded first

;; Load the rules (use load for relative path w.r.t. to that file)
(load "../meta-rules/conditional-instantiation-meta-rule.scm")
(load "../rules/fuzzy-conjunction-introduction-rule.scm")

;; Define the rule base ci-rbs by inheriting from the predefined top
;; rule base call "URE"
(define ci-rbs (ConceptNode "ci-rbs"))

;; Associate the rules to the rule base (with weights, their semantics
;; is currently undefined, we might settled with probabilities but it's
;; not sure)
(ure-add-rule ci-rbs conditional-full-instantiation-meta-rule-name (stv 1 1))
(ure-add-rule ci-rbs fuzzy-conjunction-introduction-2ary-rule-name (stv 1 1))

;; termination criteria parameters
(ure-set-maximum-iterations ci-rbs 20)

;; Attention allocation (set the TV strength to 0 to disable it, 1 to
;; enable it)
(ure-set-attention-allocation ci-rbs #f)
