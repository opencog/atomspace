;
; Some trivial sequences.

(use-modules (opencog))

(define get-something (Query (True) (Concept "it's true")))
(define get-nothing   (Query (False) (Concept "it's false")))
(define get-not-true  (Query (Not (True)) (Concept "it's not true")))
(define get-not-false (Query (Not (False)) (Concept "it's not false")))
