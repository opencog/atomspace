;
; Configuration file for PLN unit test (used by
; BackwardChainerUTest.cxxtest)
;
; To be loaded first

; Load the rules (use load for relative path w.r.t. to that file)
(load "bc-modus-ponens.scm")
(load "bc-deduction.scm")

; Associate the rules to the rule base (with weights, their semantics
; is currently undefined, we might settled with probabilities but it's
; not sure)
(MemberLink
   pln-rule-modus-ponens-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   pln-rule-deduction-name
   (ConceptNode "URE")
)

; termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "20")
)

; Attention allocation (set the TV strength to 0 to disable it, 1 to
; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   (ConceptNode "URE")
)
