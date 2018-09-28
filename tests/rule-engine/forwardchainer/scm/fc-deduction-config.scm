;;
;; Configuration file for the example crisp rule base system (used by
;; fc-deduction.scm)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load required modules and utils ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (opencog))
(use-modules (opencog rule-engine))

;; Useful to run the unit tests without having to install opencog
(load-from-path "rule-engine-utils.scm")

;;;;;;;;;;;;;;;;
;; Load rules ;;
;;;;;;;;;;;;;;;;
(load-from-path "tests/rule-engine/rules/fc-deduction-rule.scm")

;; Define a new rule base (aka rule-based system)
(define fc-deduction-rbs (ConceptNode "fc-deduction-rule-base"))
(InheritanceLink
   fc-deduction-rbs
   (ConceptNode "URE")
)

;; Associate the rules to the rule base (with weights, their semantics
;; is currently undefined, we might settled with probabilities but it's
;; not sure)
(ure-add-rules fc-deduction-rbs (list fc-deduction-rule-name))

;; Termination criteria parameters
(ure-set-num-parameter fc-deduction-rbs "URE:maximum-iterations" 20)

;; Attention allocation (set the TV strength to 0 to disable it, 1 to
;; enable it)
(ure-set-fuzzy-bool-parameter fc-deduction-rbs "URE:attention-allocation" 0)
