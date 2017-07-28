;
; similarity-api.scm
;
; Provide framework to fetch/store similarity values.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; There is a generic need to work with similarity values between pairs
; of things. These are usually CPU-intensive to compute, and are usually
; non-sparse: i.e. the similarity between N items requires an NxN
; matrix.  The below provides an API to work with similarities.
; Specifically, it provides an API so that similarities can be stored
; in the atomspace (i.e. so that they don't need to be recomputed).
;
; It is assumed that similarity scores are symmetric, so that exchanging
; left and right give the same answer.  Thus, an UnorderedLink is best for
; storing the pair. Specifically, the SimilarityLink.  So,
;
;    SimilarityLink
;        Atom "this"
;        Atom "that"
;
; and
;
;    SimilarityLink
;        Atom "that"
;        Atom "this"
;
; are both exactly the same atom.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog persist))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to access similarity scores.
;
(define*-public (add-similarity-api LLOBJ MTM?
	#:optional (ID #f)
"
  add-similarity-api - Add API to access similarity values between
  rows or columns of the LLOBJ.  This creates a new NON-sparse matrix
  that can be understood as a kind-of matrix product of LLOBJ with
  it's transpose.

  If MTM? is #t, then the similarity matrix is the product M^T M
  for LLOBJ = M, otherwise, the product is MM^T.  Here, M^T is the
  matrix-transpose.

  If the optional argument ID is present, it is used to cosntruct the
  key under which the similarity scores are accessed.  This is needed
  if the similarity scores are not the default cosine similarity scores.
"
	; We need 'left-basis, provided by add-pair-stars
	(let ((wldobj (add-pair-stars LLOBJ)))

		(define name
			(if ID
				(string-append "Similarity Matrix " ID)
				(string-append
					(if MTM? "Left" "Right")
					(" Cosine Similarity Matrix"))))

		; The type of the rows and columns in the composite matrix.
		(define item-type
			(if MTM?
				(LLOBJ 'right-type)
				(LLOBJ 'left-type)))

		(define pair-sim-type 'SimilarityLink)

		(define sim-key (PredicateNode
			(if ID
				(string-append "*-SimKey " ID)
				"*-Cosine Sim Key-*")))

		; Return the precomputed similarity on ATOM
		(define (get-sim ATOM)
			(if (null? ATOM) 0
				(let ((val (cog-value ATOM sim-key)))
					(if (null? val) 0
						(cog-value-ref val 0)))))

		; Save a precomputed similarity on ATOM
		(define (set-sim ATOM SIM)
			(cog-set-value! ATOM sim-key (FloatValue SIM)))

		; fetch-sim-pairs - fetch all SimilarityLinks from the database.
		(define (fetch-sim-pairs)
			(define start-time (current-time))
			(load-atoms-of-type pair-sim-type)
			(format #t "Elapsed time to load sims: ~A secs\n"
				(- (current-time) start-time)))

		; Methods on this class.
		(lambda (message . args)
			(case message
				((name)           name)
				((left-type)      item-type)
				((right-type)     item-type)
				((pair-type)      pair-sim-type)
				((fetch-pairs)    (fetch-sim-pairs))

				((pair-similarity)     (apply get-sim args))
				((set-pair-similarity) (apply set-sim args))

				; (else             (apply LLOBJ (cons message args)))
				(else (error "Bad method call on similarity API:" message))
		)))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to batch-compute similarity
; scores.
;
(define*-public (batch-similarity LLOBJ MTM?
	#:optional
	(ID #f)
	(CUTOFF 0.5)
	(SIM-FUN
		(if MTM?
			(lambda (x y) (LLOBJ 'left-cosine x y))
			(lambda (x y) (LLOBJ 'right-cosine x y))))
	)
"
  batch-similarity - Add API to batch-compute similarity values between
  rows or columns of the LLOBJ.  This creates a new NON-sparse matrix
  that can be understood as a kind-of matrix product of LLOBJ with it's
  transpose.

  If MTM? is #t, then the similarity matrix is the product M^T M
  for LLOBJ = M, otherwise, the product is MM^T.  Here, M^T is the
  matrix-transpose.
"
	; We need 'left-basis, provided by add-pair-stars
	(let* ((wldobj (add-pair-stars LLOBJ))
			(simobj (add-similarity-api wldobj))
			(pair-sim-type (simobj 'pair-type))
		)

		; Fetch or compute the similarity value.
		; If the sim value is stored already, return that,
		; else compute it. If the computed value is greater than
		; CUTOFF, then save it.
		(define (compute-sim A B)
			(define mpr (cog-link pair-sim-type A B))
			(if (not (null? mpr))
				(sim-obj 'pair-similarity mpr)
				(let ((simv (SIM-FUN A B)))
					(if (< 0.5 simv)
						(sim-obj 'set-pair-similarity (cog-new-link pair-sim-type A B) simv))
					simv)))

		; Compute and store the similarity between the ITRM, and the
		; other items in the ITEM-LIST.  Do NOT actually cache the
		; similarity value, if it is less than CUTOFF.  This is used
		; to avoid having N-squared pairs cluttering the atomspace.
		;
		(define (batch-simlist ITEM ITEM-LIST)
			(for-each
				(lambda (item) (compute-sim ITEM item))
				WORD-LIST))

		; batch-sim-pairs - batch compute a bunch of them.
		(define (batch-sim-pairs)
			(define 
		)

		; Methods on this class.
		(lambda (message . args)
			(case message

				((compute-similarity)  (apply compute-sim args))

				; (else             (apply LLOBJ (cons message args)))
				(else (error "Bad method call on similarity API:" message))
		)))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------

; ---------------------------------------------------------------------

; Loop over the entire list of words, and compute similarity scores
; for them.  This might take a very long time!
; Serial version, see also parallel version below.
(define (batch-sim-pairs WORD-LIST CUTOFF)

	(define len (length WORD-LIST))
	(define tot (* 0.5 len (- len 1)))
	(define done 0)
	(define prs 0)
	(define prevf 0)
	(define start (current-time))
	(define prevt start)

	(define (do-one-and-rpt WRD-LST)
		(set! prs (+ prs (batch-sim (car WRD-LST) (cdr WRD-LST) CUTOFF)))
		(set! done (+  done 1))
		(if (eqv? 0 (modulo done 10))
			(let* ((elapsed (- (current-time) start))
					(togo (* 0.5 (- len done) (- len (+ done 1))))
					(frt (- tot togo))
					(rate (* 0.001 (/ (- frt prevf) (- elapsed prevt))))
					)
				(format #t
					 "Done ~A/~A frac=~5f% Time: ~A Done: ~4f% rate=~5f K prs/sec\n"
					done len
					(* 100.0 (/ prs frt))
					elapsed
					(* 100.0 (/ frt tot))
					rate
				)
				(set! prevt (- elapsed 1.0e-6))
				(set! prevf frt)
		)))

	; tail-recursive list-walker.
	(define (make-pairs WRD-LST)
		(if (not (null? WRD-LST))
			(begin
				(do-one-and-rpt WRD-LST)
				(make-pairs (cdr WRD-LST)))))

	(make-pairs WORD-LIST)
)

; ---------------------------------------------------------------------

; Loop over the entire list of words, and compute similarity scores
; for them.  Hacked parallel version.
(define (para-batch-sim-pairs WORD-LIST CUTOFF)

	(define len (length WORD-LIST))
	(define tot (* 0.5 len (- len 1)))
	(define done 0)
	(define prs 0)
	(define prevf 0)
	(define start (current-time))
	(define prevt start)

	(define nthreads 3)

	(define (do-one-and-rpt WRD-LST)
		; These sets are not thread-safe but I don't care.
		(set! prs (+ prs (batch-sim (car WRD-LST) (cdr WRD-LST) CUTOFF)))
		(set! done (+ done 1))
		(if (eqv? 0 (modulo done 20))
			(let* ((elapsed (- (current-time) start))
					(togo (* 0.5 (- len done) (- len (+ done 1))))
					(frt (- tot togo))
					(rate (* 0.001 (/ (- frt prevf) (- elapsed prevt))))
					)
				(format #t
					 "Done ~A/~A frac=~5f% Time: ~A Done: ~4f% rate=~5f K prs/sec\n"
					done len
					(* 100.0 (/ prs frt))
					elapsed
					(* 100.0 (/ frt tot))
					rate
				)
				(set! prevt (- elapsed 1.0e-6))
				(set! prevf frt)
			)))

	; tail-recursive list-walker.
	(define (make-pairs WRD-LST)
		(if (not (null? WRD-LST))
			(begin
				(do-one-and-rpt WRD-LST)
				(if (< nthreads (length WRD-LST))
					(make-pairs (drop WRD-LST nthreads))))))

	; thread launcher
	(define (launch WRD-LST CNT)
		(if (< 0 CNT)
			(begin
				(call-with-new-thread (lambda () (make-pairs WRD-LST)))
				(launch (cdr WRD-LST) (- CNT 1)))))

	(launch WORD-LIST nthreads)

	(format #t "Started ~d threads\n" nthreads)
)

; ---------------------------------------------------------------------
; Example usage:
;
; (use-modules (opencog) (opencog persist) (opencog persist-sql))
; (use-modules (opencog nlp) (opencog nlp learn))
; (sql-open "postgres:///en_pairs_sim?user=linas")
; (sql-open "postgres:///en_pairs_supersim?user=linas")
; (use-modules (opencog cogserver))
; (start-cogserver "opencog2.conf")
; (fetch-all-words)
; (define pca (make-pseudo-cset-api))
; (pca 'fetch-pairs)
; (define ac (get-all-cset-words))
; (length ac)
; 37413
; (define ad (get-all-disjuncts))
; (length ad)
; 291637
;
; (define firm (filter (lambda (wrd) (< 8.0 (cset-vec-word-len wrd))) ac))
; (length firm)
; 1985
;
; (batch-sim-pairs firm)
