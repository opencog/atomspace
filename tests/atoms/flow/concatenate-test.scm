;
; concatenate-test.scm -- Verify that ConcatenateLink works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "concatenate-test")
(test-begin tname)

; -------------------------------------------------------------
; Base function: flatten set of lists

(define flatten-set
	(Concatenate
		(Type 'SetLink)
		(List
			(List (Concept "a") (Concept "b"))
			(List (Concept "c") (Concept "b")))))

(define flat-set (cog-execute! flatten-set))
(format #t "flat set is ~A" flat-set)

(test-assert "flat set"
	(equal? flat-set
		(Set (Concept "c") (Concept "a") (Concept "b"))))

(test-end tname)

(opencog-test-end)
