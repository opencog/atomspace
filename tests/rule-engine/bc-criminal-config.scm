;;
;; Configuration file for PLN unit test (used by
;; BackwardChainerUTest.cxxtest)
;;
;; To be loaded first

;; Load the rules (use load for relative path w.r.t. to that file)
(load-from-path "tests/rule-engine/meta-rules/conditional-instantiation-meta-rule.scm")
(load-from-path "tests/rule-engine/rules/bc-deduction-rule.scm")
(load-from-path "tests/rule-engine/rules/conjunction-fuzzy-evaluation-rule.scm")

;; Associate the rules to the rule base (with weights, their semantics
;; is currently undefined, we might settled with probabilities but it's
;; not sure)
(MemberLink (stv 1 1)
   conditional-full-instantiation-meta-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   bc-deduction-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   conjunction-fuzzy-evaluation-1ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   conjunction-fuzzy-evaluation-2ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   conjunction-fuzzy-evaluation-3ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   conjunction-fuzzy-evaluation-4ary-rule-name
   (ConceptNode "URE")
)
(MemberLink (stv 1 1)
   conjunction-fuzzy-evaluation-5ary-rule-name
   (ConceptNode "URE")
)

;; termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   (ConceptNode "URE")
   (NumberNode "400")
)

;; Attention allocation (set the TV strength to 0 to disable it, 1 to
;; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   (ConceptNode "URE")
)

;; termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:BC:complexity-penalty")
   (ConceptNode "URE")
   (NumberNode "0.001")
)
