;
; cosine-api.scm
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
(use-modules (opencog))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to loop over all pairs
; in a matrix.
;
(define-public (add-similarity-api LLOBJ)
"
  add-similarity-api - Add API to batch-compute and access similarity
  values between rows or columns of the LLOBJ.  This creates
  a new NON-sparse matrix that can be understood as a kind-of matrix
  product of LLOBJ with it's transpose.

  If TRANSP? is #f, then the matrix 
"
	; We need 'left-basis, provided by add-pair-stars
	(let ((wldobj (add-pair-stars LLOBJ)))

		(define pair-sim-type 'SimilarityLink)
		(define cos-key (PredicateNode "*-Cosine Distance Key-*"))

		; fetch-sim-pairs - fetch all SimilarityLinks from the database.
		(define (fetch-sim-pairs)
			(define start-time (current-time))
			(load-atoms-of-type pair-sim-type)
			(format #t "Elapsed time to load sims: ~A secs\n"
				(- (current-time) start-time)))

		; Methods on this class.
		(lambda (message . args)
			(case message
				((pair-type)      pair-sim-type)
				((fetch-pairs)    (fetch-sim-pairs))

				; (else             (apply LLOBJ (cons message args))))
				(else (error "Bad method call on similarity API:" message))))
		))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
