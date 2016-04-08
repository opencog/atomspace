;
; Simple crisp deduction example for testing forward chaining
;

(use-modules (opencog))
(use-modules (opencog rule-engine))

; Load URE configuration (add the current file dir so it can be loaded
; from anywhere)
; (add-to-load-path (dirname (current-filename)))
(load "fc-deduction-config.scm")

; Define knowledge base
(define A (ConceptNode "A" (stv 1 1)))
(define B (ConceptNode "B"))
(define C (ConceptNode "C"))
(define AB (InheritanceLink (stv 1 1) A B))
(define BC (InheritanceLink (stv 1 1) B C))
