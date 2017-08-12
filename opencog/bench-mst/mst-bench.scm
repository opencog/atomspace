;
; mst-bench.scm
;
; A sloppy, cheesey, quick-n-dirty tool to measure MST performance.
; Linas Vepstas August 2017
;

(use-modules (opencog) (opencog sheaf))

; Create a random vertex, drawing from a vocabulary having a mean
; of NVERTS different "words".
; Uses an exponential distribution to cheesily approximate a
; Zipfian distribution.
(define (mkvert NVERTS)
	(Concept (format #f "vertex ~D"
		(round (inexact->exact (* NVERTS (random:exp)))))))

(define mi-pred (Predicate "Faux MI Key"))

; Create MPAIRS different random pairs, connecting NVERTS different
; vertexes.
(define (make-pairs MPAIRS NVERTS)
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

; Create a scoring function that returns the MI, if the pair exists.
(define (score-faux LEFT RIGHT DIST)
	(define llpr (cog-link 'ListLink LEFT RIGHT))
	(if (null? llpr) -10000.0
		(cog-value-ref
			(cog-value (Evaluation (Predicate "benchy") llpr) mi-pred) 0))
)

; A performance measurement function
(define (report-rate THUNK CNT)
	(define start (get-internal-real-time))
	(let ((foo (THUNK)))
		(define ela (exact->inexact (/ (- (get-internal-real-time) start)
			 internal-time-units-per-second)))
		(format #t "Performed ~D ops in ~A seconds, at ~A ops/sec\n"
			CNT ela (/ CNT ela))))

; Step 1: create a bunch of random pair data.
(define nvocab 5000)
(define npairs 101000)
(report-rate (lambda () (make-pairs npairs nvocab)) 1)

(report-rate
	(lambda ()
		(mst-parse-atom-seq (mksent 10 30) score-faux)) 1)
