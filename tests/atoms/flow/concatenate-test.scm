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
		(Set (Concept "b") (Concept "c") (Concept "a") (Concept "b"))))

; -------------------------------------------------------------
; Base function: flatten LinkValues
(define link-link
	(LinkValue
		(LinkValue (Concept "a") (Concept "b"))
		(LinkValue (Concept "c") (Concept "b"))))

(cog-set-value! (Anchor "rock") (Predicate "key") link-link)

(define flatten-val
	(Concatenate
		(ValueOf (Anchor "rock") (Predicate "key"))))

(define flat-val (cog-execute! flatten-val))
(format #t "flat val is ~A" flat-val)

(test-assert "flat val"
	(equal? flat-val
		(LinkValue (Concept "a") (Concept "b") (Concept "c") (Concept "b"))))

; -------------------------------------------------------------

(test-end tname)
(opencog-test-end)
