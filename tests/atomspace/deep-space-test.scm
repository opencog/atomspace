;
; deep-space-test.scm
; Assorted tests of change-sets in the atomspace.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(opencog-test-runner)

; -------------------------------------------------------------------

(define vstack "simple value stack")
(test-begin vstack)

(define base-space (cog-atomspace))

; Create a list of atomspaces, each a child of the last.
(define (make-space-list LST NUM)
	(if (<= NUM 0) LST
		(let ((newspace (cog-new-atomspace)))
			(cog-set-atomspace! newspace)
			(make-space-list (cons newspace LST) (- NUM 1)))))

; Twenty of them, the base space first in the list.
(define space-list (reverse (make-space-list (list base-space) 20)))



(test-end vstack)
