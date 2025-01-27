;
; link-column-test.scm -- Verify that LinkColumn works.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "link-column-test")
(test-begin tname)

; ------------------------------------------------------------
; Serialize in list form.

(define itli (list
	(Item "a")
	(Item "b")
	(Item "c")
	(Item "d")))
(define itcol (LinkColumn (List itli)))
(define itvec (cog-execute! itcol))
(format #t "Item list vect: ~A\n" itvec)
(test-assert "Item list vect" (equal? itvec (LinkValue itli)))

; ------------------------------------------------------------
; Serialize numbers, in direct form.

(define dircol (LinkColumn itli))
(define dirvec (cog-execute! dircol))
(format #t "Direct item vect: ~A\n" dirvec)
(test-assert "Direct vect" (equal? dirvec (LinkValue itli)))

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
