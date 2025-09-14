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
; Make sure numeric equality actually works.

(test-assert "equality"
	(equal?
		(FloatValue 0 1 2 2 3 9999)
		(FloatValue 0 1 2 2 3 9999)))

(test-assert "cog equality"
	(cog-equal?
		(FloatValue 0 1 2 2 3 6666)
		(FloatValue 0 1 2 2 3 6666)))

(test-assert "non-equality"
	(not (equal?
		(FloatValue 0  1  2 2 3  123456)
		(FloatValue 0 -1 -2 2 3 -123456))))

(test-assert "cog non-equality"
	(not (cog-equal?
		(FloatValue 0  1  2 2 3  123456)
		(FloatValue 0 -1 -2 2 3 -123456))))

(test-assert "near-not-equality"
	(not (equal?
		(FloatValue 1.0e-12)
		(FloatValue 1.0e-13))))

(test-assert "small-near-not-equality"
	(not (equal?
		(FloatValue 1.0e-30)
		(FloatValue -1.0e-30))))

(test-assert "very-near-not-equality"
	(not (equal?
		(FloatValue 1.0e-130)
		(FloatValue -1.0e-130))))

(test-assert "very-offset-not-equality"
	(not (equal?
		(FloatValue 1.0e-130)
		(FloatValue -1.0000000001e-130))))

(test-assert "very-near-still-no-equality"
	(not (equal?
		(FloatValue -1.0e-130)
		(FloatValue -1.0000000001e-130))))

; These two differ by five ULPS. Current code asks
; for equality to 24 ULP or better, so these cound as equal.
(test-assert "very-near-equality"
	(equal?
		(FloatValue -1.0e-130)
		;              123456789012345
		(FloatValue -1.000000000000001e-130)))

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

(define (wtf x) (format #t "Minus: ~A\n" x) x)
(test-assert "pair diff"
	(equal? (FloatValue 0 1 2 2 3)
		(wtf (cog-execute! (Minus pair-location)))))

(test-assert "pair element diff"
	(equal? (FloatValue 0 1 2 2 3)
		(cog-execute! (Minus
			(ElementOf (Number 0) pair-location)
			(ElementOf (Number 1) pair-location)))))

(test-assert "pair times"
	(equal? (FloatValue 1 2 3 8 10)
		(cog-execute! (Times pair-location))))

(test-assert "pair divide"
	(equal? (FloatValue 1 2 3 2 2.5)
		(cog-execute! (Divide pair-location))))

; Slightly more complex division
(cog-set-value! (Anchor "location") (Predicate "vector-pairs")
	(LinkValue
		(FloatValue 1 1 1 2 2)
		(FloatValue 1 2 3 4 5)))

(test-assert "pair fraction"
	(equal? (FloatValue 1 0.5 (/ 1.0 3.0) 0.5 0.4)
		(cog-execute! (Divide pair-location))))

; -----------------------------------------------
(test-end tname)
(opencog-test-end)
