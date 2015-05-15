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
; scheme@(guile-user)> (load-from-path "simple-deduction.scm")
;
; 3. Scroll to the bottom, and run some of the commented-out examples.

(use-modules (opencog))
(use-modules (opencog rule-engine))

(load-from-path "utilities.scm")
(load-from-path "av-tv.scm")

(define A (ConceptNode "A"))
(define B (ConceptNode "B"))
(define C (ConceptNode "C"))
(define AB (ImplicationLink (stv 1 1) A B))
(define BC (ImplicationLink (stv 1 1) B C))

; (cog-fc AB "examples/rule-engine/cpolicy.json")

; Expected output should be
; TODO
