;
; tv-test.scm -- copy of demo example truthvalues.scm
;
(use-modules (opencog))

; TruthValues normally consist of two floats:
; by convention, a "strength" and a "confidence".
(define tv (SimpleTruthValue 0.1 0.2))

; A truth value can be converted to a scheme list
(cog-value->list tv)

; Alternately, individual elements in the list can be accessed directly.
; This behaves just like   (list-ref (cog-value->list VAL) NUM)
; but is computationally faster.
(cog-value-ref tv 0)
(cog-value-ref tv 1)

; TruthValues can be attached to atoms:
(define a (Concept "aaa"))
(cog-set-tv! a tv)

; The attached truth value can be fetched.
(cog-tv a)

(cog-set-tv! (Concept "bbb") (CountTruthValue 1.0e-6 -19.9316 55))
