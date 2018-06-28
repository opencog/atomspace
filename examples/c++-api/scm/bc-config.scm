; Load the rules (use load for relative path w.r.t. to that file)
(load "modus-ponens.scm")
(load "deduction.scm")

; Associate the rules to the rule base (with weights, their semantics
; is currently undefined, we might settled with probabilities but it's
; not sure)
(MemberLink
   (ConceptNode "BC_DEMO_RB")
   pln-rule-modus-ponens-name
)
(MemberLink (stv 1 1)
   (ConceptNode "BC_DEMO_RB")
   pln-rule-deduction-name
)

; termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "BC_DEMO_RB")
   (NumberNode "100")
)

; Attention allocation (set the TV strength to 0 to disable it, 1 to
; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "BC_DEMO_RB:attention-allocation")
   (ConceptNode "BC_DEMO_RB")
)
