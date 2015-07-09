;
; Configuration file for the example crisp rule base system (used by
; simple-deduction.scm)
;
; Before running any inference you must load that file

; Load the rules (use load for relative path w.r.t. to that file)
(load "rules/crisp-modus-ponens.scm")
(load "rules/crisp-deduction.scm")

; Define a new rule base (aka rule-based system)
(define simple-crisp-rbs (ConceptNode "crisp-rule-base"))
(InheritanceLink
   simple-crisp-rbs
   (ConceptNode "URE")
)

; Create helper functions to call the forward and backward chainer on
; that system
(define (simple-crisp-fc source) (cog-fc source simple-crisp-rbs))
(define (simple-crisp-bc target) (cog-bc target simple-crisp-rbs))

; Associate the rules to the rule base (with weights, their semantics
; is currently undefined, we might settled with probabilities but it's
; not sure)
(define crisp-rules (list (list crisp-modus-ponens 0.4)
                          (list crisp-deduction 0.6))
)
(ure-add-rules simple-crisp-rbs crisp-rules)

; Termination criteria parameters
(ure-set-num-parameter simple-crisp-rbs "URE:maximum-iterations" 20)

; Attention allocation (set the TV strength to 0 to disable it, 1 to
; enable it)
(ure-set-fuzzy-bool-parameter simple-crisp-rbs "URE:attention-allocation" 0)
