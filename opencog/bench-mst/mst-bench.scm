;
; mst-bench.scm
;
; A sloppy, cheesey, quick-n-dirty tool to measure MST performance.
; Linas Vepstas August 2017
;

(use-modules (opencog) (opencog sheaf))

; Create a random vertex, drawing from a vocabulary having a mean
; of NVERTS.
; Uses an exponential distribution to cheesily approximate a
; Zipfian distribution.
(define (mkvert NVERTS)
	(Concept (format #f "vertex ~D"
		(round (inexact->exact (* NVERTS (random:exp)))))))

; Step 1: create a bunch of random pair data.

(define mi-pred (Predicate "Faux MI Key"))

; Create MPAIRS different random pairs, connecting NVERTS different
; vertexes.
(define (make-words MPAIRS NVERTS)
	(define (mkpair)
		(Evaluation (Predicate "benchy") (List (mkvert NVERTS) (mkvert NVERTS))))

	; Use a Gaussian distribution, centerd around 2, having a standard
	; deviation of about 5. This is close to reality for English
	; word-pairs, I think.
	(define (mkmi) (+ 4 (* 5 (random:normal))))

	; Create a random pair, and assign a random weight to it.
	(define (mkwpair)
		(cog-set-value! (mkpair) mi-pred (FloatValue (mkmi))))

	;; Loop M times.
	(define (mkmprs M)
		(if (< 0 M) (begin (mkwpair) (mkmprs (- M 1)))))

	(mkmprs MPAIRS)
)

; Create a "sentence" that is of length LEN, and draws from a vocabulary
; of NVERTS.
(define (mksent LEN NVERTS)
	(define (mkwlist lst len)
		(if (< 0 len)
			(mkwlist (cons (mkvert NVERTS) lst) (- len 1))
			lst))
	(mkwlist '() LEN)
)

(define (score-faux PAIR)
	42
)
