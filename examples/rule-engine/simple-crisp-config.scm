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
(MemberLink (stv 0.4 1)
   crisp-modus-ponens
   simple-crisp-rbs
)
(MemberLink (stv 0.6 1)
   crisp-deduction
   simple-crisp-rbs
)

; Termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   simple-crisp-rbs
   (NumberNode "20")
)

; Attention allocation (set the TV strength to 0 to disable it, 1 to
; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   simple-crisp-rbs
)
