;
; Simple crisp deduction example
;
; 1. Launch guile under this directory (see examples/guile/README.md
; for prerequisite if you haven't already)
;
; $ guile
;
; 2. The load this file
;
; scheme@(guile-user)> (load-from-path "crisp.scm")
;
; 3. Scroll to the bottom, and run some of the commented-out examples.

(use-modules (opencog))
(use-modules (opencog rule-engine))

; Load URE configuration (add the current file dir so it can be loaded
; from anywhere)
; (add-to-load-path (dirname (current-filename)))
(load "crisp-config.scm")

; Define knowledge base
(define A (PredicateNode "A" (stv 1 1)))
(define B (PredicateNode "B"))
(define C (PredicateNode "C"))
(define AB (ImplicationLink (stv 1 1) A B))
(define BC (ImplicationLink (stv 1 1) B C))

; 1. Test forward chaining (based on the deduction rule and modus ponens)

;; scheme@(guile-user)> (crisp-fc AB)
;; $1 = (SetLink
;;    (ImplicationLink (stv 1 0.99999982)
;;       (PredicateNode "A" (stv 1 0.99999982))
;;       (PredicateNode "C" (stv 1 0.99999982))
;;    )
;;    (PredicateNode "B" (stv 1 0.99999982))
;;    (PredicateNode "C" (stv 1 0.99999982))
;; )

; 2. Test backward chaining (based on the modus ponens rule)

;; scheme@(guile-user)> (crisp-bc C)
;; $2 = (SetLink
;;    (PredicateNode "C" (stv 1 1))
;; )
