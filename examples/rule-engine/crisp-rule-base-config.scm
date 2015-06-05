;
; Configuration file for the example crisp rule base system (used by
; simple-deduction.scm)
;
; Before running any inference you must load that file

; Load the rules (use load for relative path w.r.t. to that file)
(load "rules/crisp-modus-ponens.scm")
(load "rules/crisp-deduction.scm")

; Define a new rule base (aka rule-based system)
(InheritanceLink
   (ConceptNode "crisp-rule-base")
   (ConceptNode "URE")
)

; Associate the rules to the rule base (with weights, their semantics
; is currently undefined, we might settled with probabilities but it's
; not sure)
(MemberLink (stv 0.4 1)
   (Node "crisp-rule-base-modus-ponens-rule")
   (ConceptNode "crisp-rule-base")
)
(MemberLink (stv 0.6 1)
   (Node "crisp-rule-base-deduction-rule")
   (ConceptNode "crisp-rule-base")
)

; Termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "crisp-rule-base")
   (NumberNode "20")
)

; Attention allocation (set the TV strength to 0 to disable it, 1 to
; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   (ConceptNode "crisp-rule-base")
)
