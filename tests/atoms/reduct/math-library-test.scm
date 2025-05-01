;
; math-library-test.scm -- Test assorted elementary arithmetic functions
;
; To run by hand, just say `guile -s math-library-test.scm`.
;

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
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

; -----------------------------------------------
; Test SineLink

(define pi 3.141592653589793238462643383279502884)

#! =========
; Hmmm ... should differ by only a few ULPS but doesn't ...
(test-assert "sine n pi"
	(equal? (Number 1 1 1 1 1)
		(cog-execute!
			(Plus
				(Sine (Number 0 pi (* 2 pi) (* 3 pi) (* 4 pi)))
				(Number 1 1 1 1 1))
		)))

	(cog-evaluate!
		(Equal
			(Number 1 1 1 1 1)
			(Plus
				(Sine (Number 0 pi (* 2 pi) (* 3 pi) (* 4 pi)))
				(Number 1 1 1 1 1))))
======= !#

(define sero
	(cog-execute! (Sine (Number 0 pi (* 2 pi) (* 3 pi) (* 4 pi)))))

(for-each
	(lambda (x)
		(test-approximate "sine zero" 0.0 x 2e-14))
	(cog-value->list sero))

(define (npih n) (* n 0.5 pi))

(test-assert "sine n pi-half"
	(equal? (Number 1 -1 1 -1 1)
		(cog-execute! (Sine (Number
			(npih 1) (npih 3) (npih 5) (npih 7) (npih 9))))))

; -----------------------------------------------
; Test Heaviside and SineLink

(for-each
	(lambda (n)
		(define sn (cog-execute! (Heaviside (Sine (Number n)))))

		; Ugh. round down. Even or odd, then reverse.
		(define wv (Number (- 1 (modulo (floor (/ n pi)) 2))))
		(test-assert "square-wave" (equal? sn wv))
	)
	(iota 70))

; -----------------------------------------------
; Test CosineLink

(test-assert "cosine n pi"
	(equal? (Number 1 -1 1 -1 1)
		(cog-execute!  (Cosine (Number 0 pi (* 2 pi) (* 3 pi) (* 4 pi))))))

(define cero
	(cog-execute! (Cosine (Number
		(npih 1) (npih 3) (npih 5) (npih 7) (npih 9)))))

(for-each
	(lambda (x)
		(test-approximate "cosine zero" 0.0 x 2e-14))
	(cog-value->list cero))

; -----------------------------------------------
; Test FloorLink

(for-each
	(lambda (n)
		(define x (+ 0.05 (* n 0.1)))
		(define fl (cog-execute! (Floor (Number x))))
		(define ex (Number (floor x)))

		(test-assert "square-wave" (equal? fl ex))
	)
	(iota 70))

(test-end tname)

(opencog-test-end)
