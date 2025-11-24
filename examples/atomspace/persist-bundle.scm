;
;
; several at once
;
(use-modules (opencog) (opencog persist) (opencog persist-rocks))

(define as-main (cog-atomspace))
(define as-one (AtomSpace "foo"))
(define as-two (cog-new-atomspace))



