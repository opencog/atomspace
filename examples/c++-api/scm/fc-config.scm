; Load the rules (use load for relative path w.r.t. to that file)
(load "deduction.scm")

; Associate the rules to the rule base (with weights, their semantics
; is currently undefined, we might settled with probabilities but it's
; not sure)
(MemberLink (stv 1 1)
   pln-rule-deduction-name
   (ConceptNode "FC_DEMO_RB")
)

; termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "FC_DEMO_RB")
   (NumberNode "100")
)

; Attention allocation (set the TV strength to 0 to disable it, 1 to
; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "FC_DEMO_RB:attention-allocation")
   (ConceptNode "FC_DEMO_RB")
)
