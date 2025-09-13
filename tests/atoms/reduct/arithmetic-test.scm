;
; arithmetic-test.scm -- Test basic arithmetic
;
; To run by hand, just say `guile -s arithmetic-test.scm`.
;

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "arithmetic-test")
(test-begin tname)

; -----------------------------------------------
; Test fetching pairs of vectors from location

(cog-set-value! (Anchor "location") (Predicate "vector-pairs")
	(LinkValue
		(FloatValue 1 2 3 4 5)
		(FloatValue 1 1 1 2 2)))

(define pair-location 
	(FloatValueOf (Anchor "location") (Predicate "vector-pairs")))

(test-assert "pair sum"
	(equal? (FloatValue 2 3 4 6 7)
		(cog-execute! (Plus pair-location))))

(test-assert "pair element sum"
	(equal? (FloatValue 2 3 4 6 7)
		(cog-execute! (Plus
			(ElementOf (Number 0) pair-location)
			(ElementOf (Number 1) pair-location)))))

(test-assert "pair diff"
	(equal? (FloatValue 0 1 2 2 3)
		(cog-execute! (Minus pair-location))))

(test-assert "pair element diff"
	(equal? (FloatValue 0 1 2 2 3)
		(cog-execute! (Minus
			(ElementOf (Number 0) pair-location)
			(ElementOf (Number 1) pair-location)))))

; -----------------------------------------------
(test-end tname)
(opencog-test-end)
