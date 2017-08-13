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

; Count the total number of edges in a sentence.
(define (cnt-edges SENT)
	(define (cnt-em CNT lst)
		(define frst (car lst))
		(define rest (cdr lst))
		(define nlinks 0)
		(if (null? rest) CNT
			(begin
				(for-each
					(lambda (rght)
						(if (not (null? (cog-link 'ListLink frst rght)))
							(set! nlinks (+ 1 nlinks))))
					rest)
				(cnt-em (+ CNT nlinks) rest))))
	(cnt-em 0 SENT)
)

; Create a scoring function that returns the MI, if the pair exists.
(define (score-faux LEFT RIGHT DIST)
	(define llpr (cog-link 'ListLink LEFT RIGHT))
	(if (null? llpr) -10000.0
		(cog-value-ref
			(cog-value (Evaluation (Predicate "benchy") llpr) mi-pred) 0))
)

; Create a scoring function that returns the MI, if the pair exists.
; It rejects links that are longer than 6 lengths long.
(define (score-dist-limit LEFT RIGHT DIST)
	(if (< 6 DIST) -10000.0 (score-faux LEFT RIGHT DIST))
)

; A performance measurement function
(define (report-rate THUNK CNT)
	(define start (get-internal-real-time))
	(let ((foo (THUNK)))
		(define ela (exact->inexact (/ (- (get-internal-real-time) start)
			 internal-time-units-per-second)))
		(format #t "Performed ~D ops in ~A seconds, at ~A ops/sec\n"
			CNT ela (/ CNT ela))
		ela))

; Report performance of parsing sentences of length LEN
(define (report-perf LEN)
	; Timing loop
	(define nsents (if (< 10 LEN) (if (< 20 LEN) 140 800) 6000))

	; Step 2: Measure baseline performance of creating sentences
	(define (mksents howmany sentlen)
		(if (< 0 howmany)
			(begin
				(mksent sentlen nvocab)
				(mksents (- howmany 1) sentlen))))

	; Step 3: Measure baseline performance of MST parsing
	(define (parse-them howmany sentlen)
		(if (< 0 howmany)
			(begin
				(mst-parse-atom-seq (mksent sentlen nvocab) score-faux)
				; (mst-parse-atom-seq (mksent sentlen nvocab) score-dist-limit)
				(parse-them (- howmany 1) sentlen))))

	(define baseline (report-rate (lambda () (mksents nsents LEN)) nsents))
	(define measurme (report-rate (lambda () (parse-them nsents LEN)) nsents))

	(define adjusted-time (- measurme baseline))
	(define millisecs-per (* 1000 (/ adjusted-time nsents)))
	(format #t "Sentence length = ~D Millisecs/parse = ~8F Parses/sec = ~8F\n"
		LEN millisecs-per (/ nsents adjusted-time))
)

; OK Go -- do a bunch of them.
(define (report-all SLEN MAXLEN)
	(if (< SLEN MAXLEN)
		(begin
			(report-perf SLEN)
			(report-all (+ SLEN 1) MAXLEN))))

; Step 1: create a bunch of random pair data.

; With the below, a typical 
; 5-word sentence will have 8 edges
; 10-word sentence will have 24 edges
; 15-word sentence will have 72 edges
(define nvocab 300)
(define npairs 701000)

(format #t "Creating ~D pairs for ~D words ... one moment please...\n"
	npairs nvocab)
(report-rate (lambda () (make-pairs npairs nvocab)) 1)

(report-all 2 35)
