;
; Configuration file for the example crisp rule base system (used by
; fc.scm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load required modules and utils ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (opencog))
(use-modules (opencog rule-engine))

;; Useful to run the unit tests without having to install opencog
(load-from-path "utilities.scm")
(load-from-path "av-tv.scm")
(load-from-path "rule-engine-utils.scm")

;;;;;;;;;;;;;;;;
;; Load rules ;;
;;;;;;;;;;;;;;;;
(load-from-path "tests/rule-engine/rules/fc-modus-ponens-rule.scm")
(load-from-path "tests/rule-engine/rules/fc-deduction-rule.scm")

; Define a new rule base (aka rule-based system)
(define fc-rbs (ConceptNode "fc-rule-base"))
(InheritanceLink
   fc-rbs
   (ConceptNode "URE")
)

; Associate the rules to the rule base (with weights, their semantics
; is currently undefined, we might settled with probabilities but it's
; not sure)
(define fc-rules (list (list fc-modus-ponens-rule-name 0.4)
                       (list fc-deduction-rule-name 0.6))
)
(ure-add-rules fc-rbs fc-rules)

; Termination criteria parameters
(ure-set-num-parameter fc-rbs "URE:maximum-iterations" 20)

; Attention allocation (set the TV strength to 0 to disable it, 1 to
; enable it)
(ure-set-fuzzy-bool-parameter fc-rbs "URE:attention-allocation" 0)
