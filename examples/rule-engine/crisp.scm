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
(add-to-load-path (dirname (current-filename)))
(load-from-path "crisp-config.scm")

; Define knowledge base
(define A (ConceptNode "A" (stv 1 1)))
(define B (ConceptNode "B"))
(define C (ConceptNode "C"))
(define AB (ImplicationLink (stv 1 1) A B))
(define BC (ImplicationLink (stv 1 1) B C))

; 1. Test forward chaining (based on the deduction rule)

; (crisp-fc AB)

; Expected output should be something like
;; $1 = (ListLink
;;    (ImplicationLink (stv 1 0.99999982)
;;       (ConceptNode "A")
;;       (ConceptNode "C")
;;    )
;; )

; 2. Test backward chaining (based on the modus ponens rule)

; (crisp-bc B)

; Expected output will be empty
;; $1 = (ListLink
;; )
;; while the TV of B will be suitably updated.
