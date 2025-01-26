;
; float-column-test.scm
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

; ------------------------------------------------------------
(define num (NumberNode 1 2 3 4))
(define ncol (FloatColumn num))
(define nvec (cog-execute! ncol))
(format #t "number vect: ~A\n" nvec)

; ------------------------------------------------------------
(define numli (List
	(NumberNode 1)
	(NumberNode 2)
	(NumberNode 3)
	(NumberNode 4)))
(define nlicol (FloatColumn numli))
(define nlivec (cog-execute! nlicol))
(format #t "number list vect: ~A\n" nlivec)

; ------------------------------------------------------------
