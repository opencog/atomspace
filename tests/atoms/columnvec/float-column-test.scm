;
; float-column-test.scm
;
(use-modules (opencog) (opencog exec))

(define num (NumberNode 1 2 3 4))
(define ncol (FloatColumn num))
(define nvec (cog-execute! ncol))
(format #t "number vect: ~A\n" nvec)
