;
; Simple crisp deduction example for testing forward chaining
;

(use-modules (opencog))
(use-modules (opencog rule-engine))

; I still don't understand this module thing, it still crashes so I
; still need to have that
; (load-from-path "av-tv.scm")
;; (load-from-path "utilities.scm")
;; (load-from-path "rule-engine/rule-engine-utils.scm")

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
