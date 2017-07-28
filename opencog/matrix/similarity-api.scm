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
(use-modules (opencog))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to loop over all pairs
; in a matrix.
;
(define-public (add-similarity-api LLOBJ MTM?
	#:optional
	(ID #f)
	(CUTOFF 0.5)
	(SIM-FUN
		(if MTM?
			(lambda (x y) (LLOBJ 'left-cosine x y))
			(lambda (x y) (LLOBJ 'right-cosine x y))))
	)
"
  add-similarity-api - Add API to batch-compute and access similarity
  values between rows or columns of the LLOBJ.  This creates
  a new NON-sparse matrix that can be understood as a kind-of matrix
  product of LLOBJ with it's transpose.

  If MTM? is #t, then the similarity matrix is the product M^T M
  for LLOBJ = M, otherwise, the product is MM^T.  Here, M^T is the
  matrix-transpose.
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

		; Fetch or compute the similarity value.
		; If the sim value is stored already, return that,
		; else compute it. If the computed value is greater than
		; CUTOFF, then save it.
		(define (compute-sim A B)
			(define mpr (cog-link pair-sim-type A B))
			(if (not (null? mpr))
				(get-sim mpr)
				(let ((simv (SIM-FUN A B)))
					(if (< 0.5 simv)
						(set-sim (cog-new-link pair-sim-type A B) simv))
					simv)))

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
				((compute-similarity)  (apply compute-sim args))

				; (else             (apply LLOBJ (cons message args))))
				(else (error "Bad method call on similarity API:" message))))
		))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
