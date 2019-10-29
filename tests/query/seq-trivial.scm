;
; Some trivial sequences.

(use-modules (opencog) (opencog exec))

(define get-something (Bind (True) (Concept "it's true")))
(define get-nothing   (Bind (False) (Concept "it's false")))
(define get-not-true  (Bind (Not (True)) (Concept "it's not true")))
(define get-not-false (Bind (Not (False)) (Concept "it's not false")))
