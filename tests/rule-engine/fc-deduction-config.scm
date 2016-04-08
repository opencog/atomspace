;;
;; Configuration file for the example crisp rule base system (used by
;; fc-deduction.scm)
;;
;; Before running any inference you must load that file

;; Load the rules (use load for relative path w.r.t. to that file)
(load "rules/fc-deduction-rule.scm")

;; Define a new rule base (aka rule-based system)
(define fc-deduction-rbs (ConceptNode "fc-deduction-rule-base"))
(InheritanceLink
   fc-deduction-rbs
   (ConceptNode "URE")
)

;; Associate the rules to the rule base (with weights, their semantics
;; is currently undefined, we might settled with probabilities but it's
;; not sure)
(ure-add-rules fc-deduction-rbs (list (list fc-deduction-rule-name 0.6)))

;; Termination criteria parameters
(ure-set-num-parameter fc-deduction-rbs "URE:maximum-iterations" 20)

;; Attention allocation (set the TV strength to 0 to disable it, 1 to
;; enable it)
(ure-set-fuzzy-bool-parameter fc-deduction-rbs "URE:attention-allocation" 0)
