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

; I still don't understand this module thing, it still crashes so I
; still need to have that
(load-from-path "av-tv.scm")
(load-from-path "utilities.scm")
(load-from-path "rule-engine-utils.scm")

; Load URE configuration (add the current file dir so it can be loaded
; from anywhere)
(add-to-load-path (dirname (current-filename)))
(load-from-path "crisp-config.scm")

; Define knowledge base
(define A (ConceptNode "A" (stv 1 1)))
(define B (ConceptNode "B"))
(define C (ConceptNode "C"))
(define AB (ImplicationLink (stv 1 1) A B))
(define BC (ImplicationLink (stv 1 1) B C))

; 1. Test forward chaining (based on the deduction rule)

;; scheme@(guile-user)> (crisp-fc AB)
;; $1 = (ListLink
;;    (ImplicationLink (stv 1 0.99999982)
;;       (ConceptNode "A")
;;       (ConceptNode "C")
;;    )
;; )

; 2. Test backward chaining (based on the modus ponens rule)

;; scheme@(guile-user)> (crisp-bc C)
;; $1 = (ListLink
;; )

; while the TV of C will be suitably updated.

;; scheme@(guile-user)> C
;; $2 = (ConceptNode "C" (stv 1 0.99999982))
