;;
;; Configuration file for PLN unit test (used by
;; BackwardChainerUTest.cxxtest)
;;
;; To be loaded first

;; Load the rules (use load for relative path w.r.t. to that file)
(load "rules/implication-scope-direct-evaluation-rule.scm")
(load "meta-rules/conditional-full-instantiation-meta-rule.scm")

;; Associate the rules to the rule base (with weights, their semantics
;; is currently undefined, we might settled with probabilities but it's
;; not sure)
(MemberLink (stv 1 1)
   implication-scope-direct-evaluation-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   conditional-full-instantiation-meta-rule-name
   (ConceptNode "URE")
)

;; Termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "100")
)

;; Attention allocation (set the TV strength to 0 to disable it, 1 to
;; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   (ConceptNode "URE")
)
