;;
;; Configuration file for PLN unit test (used by
;; BackwardChainerUTest.cxxtest)
;;
;; To be loaded first

;; Load the rules (use load for relative path w.r.t. to that file)
(load-from-path "tests/ure/meta-rules/conditional-full-instantiation-meta-rule.scm")
(load-from-path "tests/ure/rules/fuzzy-conjunction-introduction-rule.scm")

(define rbs (Concept "URE"))

;; Associate the rules to the rule base
(ure-add-rule rbs conditional-full-instantiation-meta-rule-name)
(ure-add-rule rbs fuzzy-conjunction-introduction-1ary-rule-name)
(ure-add-rule rbs fuzzy-conjunction-introduction-2ary-rule-name)
(ure-add-rule rbs fuzzy-conjunction-introduction-3ary-rule-name)
(ure-add-rule rbs fuzzy-conjunction-introduction-4ary-rule-name)
(ure-add-rule rbs fuzzy-conjunction-introduction-5ary-rule-name)

;; termination criteria parameters
(ure-set-maximum-iterations rbs 20)

;; Complexity penalty
(ure-set-complexity-penalty rbs 0.1)
