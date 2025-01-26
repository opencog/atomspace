;
; sexpr-column-test.scm
;
(use-modules (opencog) (opencog exec))

; --------------------------------------------------------
; Basic test - convert list of atoms to list of strings
(define scol (SexprColumn (List (Concept "foo") (Concept "bar"))))

(define svec (cog-execute! scol))

(format #t "foobar is ~A\n" svec)

; --------------------------------------------------------
; Serialize a single node. Trival case but needs testing.
(define scolnode (SexprColumn (Concept "foo")))

(define snode (cog-execute! scolnode))

(format #t "Single node is ~A\n" snode)

; --------------------------------------------------------
; A more typical case: A list of atoms resulting from processing.

(define lv
	(LinkValue (Concept "foo") (Concept "bar") (Item "zork")))

(cog-set-value! (Anchor "heavy") (Predicate "place") lv)

(define slv
	(SexprColumn (ValueOf (Anchor "heavy") (Predicate "place"))))

(define slvec (cog-execute! slv))

(format #t "LinkValue vec ~A\n" slvec)

; --------------------------------------------------------
