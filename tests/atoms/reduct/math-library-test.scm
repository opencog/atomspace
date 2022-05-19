;
; math-library.scm -- Test assorted elementary arithmetic functions
;

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "math-library-test")
(test-begin tname)

; -----------------------------------------------
; Test Log2Link

(test-assert "log2"
	(equal? (Number 0 1 2 3 4 5)
		(cog-execute! (Log2 (Number 1 2 4 8 16 32)))))

; -----------------------------------------------
; Test PowLink
(test-assert "eight"
	(equal? (Number 8) (cog-execute! (Pow (Number 2) (Number 3)))))

(test-assert "pow three"
	(equal? (Number 8 27 64 125)
		(cog-execute! (Pow (Number 2 3 4 5) (Number 3)))))

(test-assert "two pow"
	(equal? (NumberNode 2 4 8 16)
		(cog-execute! (Pow (Number 2) (Number 1 2 3 4)))))

(test-assert "x**x"
	(equal? (NumberNode 1 4 27 256)
		(cog-execute! (Pow (Number 1 2 3 4) (Number 1 2 3 4)))))

(for-each
	(lambda (n)
		(define rp (cog-execute! (Pow (Number 2)
			(RandomNumber (Number 2) (Number 3)))))

		(define rn (car (cog-value->list rp)))
		(test-assert "2**random"
			(and (<= 4.0 rn) (<= rn 8.0))))
	(iota 30))

(test-end tname)

(opencog-test-end)
