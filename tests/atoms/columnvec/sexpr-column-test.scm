;
; sexpr-column-test.scm
;
(use-modules (opencog) (opencog exec))

(define scol
	(SexprColumn (List (Concept "foo") (Concept "bar"))))


(define svec (cog-execute! scol))

(format #t "foobar is ~A\n" svec)

(define scolnode
	(SexprColumn (Concept "foo")))

(define snode (cog-execute! scolnode))

(format #t "Single node is ~A\n" snode)

; --------------------------------------------------------
