;;
;; Configuration file for the frog example
;;
;; To be loaded first

;; Load the rules (use load for relative path w.r.t. to that file)
(load "../meta-rules/conditional-instantiation-meta-rule.scm")
(load "../rules/fuzzy-conjunction-introduction-rule.scm")

;; Define the rule base ci-rbs by inheriting from the predefined top
;; rule base call "URE"
(define ci-rbs (ConceptNode "ci-rbs"))
(Inheritance ci-rbs (ConceptNode "URE"))

;; Associate the rules to the rule base (with weights, their semantics
;; is currently undefined, we might settled with probabilities but it's
;; not sure)
(MemberLink (stv 1 1)
   conditional-full-instantiation-meta-rule-name
   ci-rbs
)
(MemberLink (stv 1 1)
   fuzzy-conjunction-introduction-2ary-rule-name
   ci-rbs
)

;; termination criteria parameters
(ExecutionLink
   (SchemaNode "URE:maximum-iterations")
   ci-rbs
   (NumberNode "20")
)

;; Attention allocation (set the TV strength to 0 to disable it, 1 to
;; enable it)
(EvaluationLink (stv 0 1)
   (PredicateNode "URE:attention-allocation")
   ci-rbs
)
