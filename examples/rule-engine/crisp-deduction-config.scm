;
; Configuration file for the example crisp rule base system (used by
; crisp-deduction.scm)
;
; Before running any inference you must load that file

; Load the rules (use load for relative path w.r.t. to that file)
(load "rules/crisp-deduction-rule.scm")

; Define a new rule base (aka rule-based system)
(define crisp-deduction-rbs (ConceptNode "crisp-deduction-rule-base"))
(InheritanceLink
   crisp-deduction-rbs
   (ConceptNode "URE")
)

; Create helper functions to call the forward and backward chainer on
; that system
(define default-focus-set (SetLink))
(define (crisp-deduction-fc source)
  (cog-fc source crisp-deduction-rbs default-focus-set))
(define (crisp-deduction-bc target)
  (cog-bc target crisp-deduction-rbs default-focus-set))

; Associate the rules to the rule base (with weights, their semantics
; is currently undefined, we might settled with probabilities but it's
; not sure)
(ure-add-rules crisp-deduction-rbs (list (list crisp-deduction-rule-name 0.6)))

; Termination criteria parameters
(ure-set-num-parameter crisp-deduction-rbs "URE:maximum-iterations" 20)

; Attention allocation (set the TV strength to 0 to disable it, 1 to
; enable it)
(ure-set-fuzzy-bool-parameter crisp-deduction-rbs "URE:attention-allocation" 0)
