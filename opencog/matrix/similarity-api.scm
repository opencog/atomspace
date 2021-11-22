;
; similarity-api.scm
;
; Provide framework to compute and hold similarity values.
;
; Copyright (c) 2017,2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; There is a generic need to work with similarity values between pairs
; of things. These are usually CPU-intensive to compute, and are usually
; non-sparse: i.e. the similarity between N items requires an NxN
; matrix.  For N=one-thousand, NxN=one-million, so this gets out of
; control pretty quickly.
;
; Two things are provided below. First, an API to set and get
; similarities.  Specifically, it provides an API for holding
; similarities as Values in the AtomSpace. This not only avoids
; recomputation, but also allows them to be persisted in the database.
;
; The second tool provided is a batch-compute function, that will
; compute all NxN similarity values. This is extremely CPU-intensive,
; and even moderate-sized matrixes can take days or weeks to compute.
;
; By default, the similarity measure is assumed to be the cosine
; similarity; the ctor for the API allows other similarity measures
; to be specified.
;
; It is assumed that similarity scores are symmetric, so that exchanging
; left and right give the same answer.  Thus, an UnorderedLink is best
; for holding the pair. Specifically, the SimilarityLink.  So,
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
; are both exactly the same atom. The actual similarity values are
; held as Values on these atoms.  The specific key used to hold
; the value depends on the arguments the API is given; by default,
; the (Predicate "*-Cosine Sim Key-*") is used; if the underlying
; matrix is filtered, then a filter-name-dependent key is used.
; Thus, the same API can be used with both filtered and non-filtered
; versions of the dataset.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (ice-9 threads)) ; for threads
(use-modules (opencog) (opencog persist))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to access similarity scores.
;
(define*-public (add-similarity-api LLOBJ
	#:optional
	(MTM? #f)
	(ID (if (LLOBJ 'filters?) (LLOBJ 'id) #f)))
"
  add-similarity-api - Add an API to access similarity values between
  two rows or columns of the LLOBJ.  This API merely provides access
  to values that were previously computed, located as Values attached
  to pairs of Atoms under certain specific keys. This API is meant
  to simiplify the naming and managment of similarity values.

  The computation of similarities between pairs of rows/columns can be
  performed with the `add-similarity-compute` object.

  Arguments:
  LLOBJ -- the matrix whose rows or columns will be compared.

  Optional arguments:
  MTM? -- If set to #t, then columns will be compared, else rows.
      If not specified, defaults to #f (rows are compared.)

  ID -- String used to generate a unique access key. The actual
      similarity values are held under a key generated using this
      string. If not specified, this defaults to the string returned
      by `(LLOBJ 'id)`.

  Provided methods:
  'set-pair-similarity PAIR VALUE -- Attach VALUE to PAIR. It is
      assumed that PAIR is a SimilarityLink (holding a pair of rows
      or columns).  The VALUE can be any Atomese Value; most users
      will use the FloatValue, but it can be anything.

  'pair-similarity PAIR -- Return the Value attached to PAIR.

  One may think of this object in either of two different ways. One
  way is to think of it as an easy way to get row or column
  similarities. Another way is to think of it as defining a new matrix
  that is a product of LLOBJ with its transpose.  The product is not
  to conventional dot-product, but rather the similarity metric.
  If MTM? is #t, then the similarity matrix is the product M^T M
  for LLOBJ = M, otherwise, the product is MM^T.  Here, M^T is the
  matrix-transpose. Fun fact: taking a more formal approach means
  that the similarity metric can be thought of as a true metric,
  used for raising and lowering covariant or contravariant indexes.
"
	; We need 'left-basis, provided by add-pair-stars
	(let ((wldobj (add-pair-stars LLOBJ)))

		(define name
			(if ID
				(string-append "Similarity Matrix " ID)
				(string-append
					(if MTM? "Left" "Right")
					" Cosine Similarity Matrix")))

		; The type of the rows and columns in the composite matrix.
		(define item-type
			(if MTM?
				(LLOBJ 'right-type)
				(LLOBJ 'left-type)))

		(define pair-sim-type 'SimilarityLink)

		(define (get-pair L-ATOM R-ATOM)
			(cog-link pair-sim-type L-ATOM R-ATOM))

		(define (make-pair L-ATOM R-ATOM)
			(SimilarityLink L-ATOM R-ATOM))

		(define sim-key (PredicateNode
			(if ID
				(string-append "*-SimKey " ID)
				"*-Similarity Key-*")))

		; Return the precomputed similarity value on ATOM
		; This returns the Value on the atom, and not a number!
		(define (get-sim ATOM)
			(if (nil? ATOM) #f
				(cog-value ATOM sim-key)))

		(define (get-pair-sim A B) (get-sim (get-pair A B)))

		; Save a precomputed similarity on ATOM. The SIM should be a
		; Value, e.g. a FloatValue. If SIM if #f then any existing
		; value is removed.
		(define (set-sim ATOM SIM)
			(cog-set-value! ATOM sim-key SIM))

		; Since SimilarityLink is an UnorderedLink, the ordering of
		; left and right will be random, depending on the atom hash.
		; However, it will be consistent within a given session.
		; Same remarks for the wildcards.
		(define (get-pair-left PAIR) (gadr PAIR))
		(define (get-pair-right PAIR) (gddr PAIR))
		(define (get-wildcard ITEM)
			(Similarity (AnyNode "wild") ITEM))
		(define (get-wild-wild)
			(Similarity (AnyNode "wild") (AnyNode "wild")))

		; fetch-sim-pairs - fetch all SimilarityLinks from the database.
		; XXX FIXME This is disasterously wrong, if the database contains
		; similarities for several different kinds of matrices in it!!
		(define (fetch-sim-pairs)
			(define elapsed-secs (make-elapsed-secs))
			(load-atoms-of-type pair-sim-type)
			(format #t "Elapsed time to load sims: ~A secs\n"
				(elapsed-secs)))

		; Methods on this class.
		(lambda (message . args)
			(case message
				((name)           name)
				((left-type)      item-type)
				((right-type)     item-type)
				((pair-type)      pair-sim-type)
				((get-pair)       (apply get-pair args))
				((make-pair)      (apply make-pair args))
				((get-count)      (apply get-sim args))
				((set-count)      (apply set-sim args))
				((pair-count)     (apply get-pair-sim args))
				((left-element)   (apply get-pair-left args))
				((right-element)  (apply get-pair-right args))
				((left-wildcard)  (apply get-wildcard args))
				((right-wildcard) (apply get-wildcard args))
				((wild-wild)      (get-wild-wild))

				((fetch-pairs)    (fetch-sim-pairs))
				((provides)       #f)
				((filters?)       #f)

				((pair-similarity)     (apply get-sim args))
				((set-pair-similarity) (apply set-sim args))

				(else (error "Bad method call on similarity API:" message))
		)))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to batch-compute similarity
; scores.
;
(define*-public (batch-similarity LLOBJ SIM-FUN
	#:optional
	(MTM? #f)
	(ID (if (LLOBJ 'filters?) (LLOBJ 'id) #f))
	)
"
  batch-similarity LLOBJ [MTM? ID CUTOFF SIM-FUN] - bulk computation.

  This adds an API for bulk computing and holding similarity values
  between two rows or columns of the LLOBJ. The computed similarity values
  will be saved in the AtomSpace via the `add-similarity-api` object.

  Batching is EXTREMELY CPU-intensive.  A typical run will take days or
  a week or more, even for modest-sized datasets. It will also blow up
  memory usage, since a SimilarityLink and also an atom Value is created
  for each pair of atoms. For N = 1000, this means N(N+1)/2 = 500K
  SimilarityLinks and values. This might require a few GBytes of RAM.

  This assumes that the support for each row or column has been
  previously computed (using the `add-support-api` object).  The
  support is needed to determine which rows/columns have the highest
  ranking.

  This assumes that the similarity metric is symmetric; that is,
  exchanging the two items in the pair give the same results. In
  other words, it is assumed that `(SIM-FUN A B)` and `(SIM-FUN B A)`
  return the same value.  This also assumes that the self-similarity
  of an item is non-trivial, so that `(SIM-FUN A A)` depends on `A`,
  and so it will loop over the diagonal entries as well.

  Arguments:
  LLOBJ -- The matrix whose rows or columns will be compared.

  SIM-FUN -- Function taking two arguments, both rows or columns, as
      appropriate, and returning a single Value. This is the function
      that will be called to compute the similarity between two rows
      or columns.

  Optional arguments:
  MTM? -- If set to #t then columns will be compared, else rows.
      If not specified, defaults to #f (compares rows.)

  ID -- named location where the similarity values should be stored.

  This creates a new NON-sparse matrix that can be understood as a
  matrix product of LLOBJ with it's transpose. The product is not
  the conventional dot-product, but rather is that defined by the
  SIM-FUN.  If MTM? is #t, then the similarity matrix is the product
  M^T M for LLOBJ = M, otherwise, the product is MM^T.  Here, M^T is
  the matrix-transpose.

  Methods provided:
  'batch-compute N -- Compute the similarity for the top-ranked N
     rows or columns in the matrix. Ranking is obtained by looking
     at the count on the support object for the row/column. A total
     of N(N+1)/2 similarity values will be computed, including the
     similarity for pairs along the diagonal. (Some kinds of similarity
     have non-trivial values along the diagonal.) The similarity is
     assumed to be symmetric; thus only the lower triangle is computed.

  'batch-list LIST -- Compute the similarity between all pairs in the
     LIST.  Thus, if the length of LIST is N, then N(N+1)/2 similarity
     values will be computed, including those along the diagonal. The
     similarity is assumed to be symmetric; thus only the lower triangle
     is computed.

  'compute-similarity A B -- Return the similarity between A and B
     (which are both either rows or columns). If the value has been
     previously computed, then that value is returned. If not, then
     it is computed by calling `SIM-FUN A B` and storing the result
     with the `add-similarity-api` object.
"
	; We need 'left-basis, provided by add-pair-stars
	(let* ((wldobj (add-pair-stars LLOBJ))
			(simobj (add-similarity-api wldobj MTM? ID))
			(compcnt 0)  ; number computed
			(savecnt 0)  ; number saved
		)

		; Find or compute the similarity value. If the sim value
		; is cached in the atomspace already, return that, else
		; compute it. If the computed value is greater than CUTOFF,
		; then cache it in the atomspace.
		(define (compute-sim A B)
			(define mpr (simobj 'get-pair A B))
			(define existing-sim (simobj 'pair-similarity mpr))
			(if (not (nil? existing-sim))
				existing-sim
				(let ((simv (SIM-FUN A B)))
					(set! compcnt (+ compcnt 1))
					(when simv
						(set! savecnt (+ savecnt 1))
						(simobj 'set-pair-similarity
							(simobj 'make-pair A B) simv))

					; If there's some existing Similarity pair,
					; but the computed similarity is bad, then
					; clobber the existing value.
					(if (and (not simv) (not (nil? mpr)))
						(simobj 'set-pair-similarity mpr #f))
					simv)))

		; Compute and cache the similarity between the ITEM, and the
		; other items in the ITEM-LIST.  Do NOT actually cache the
		; similarity value, if it is less than CUTOFF.  This is used
		; to avoid having N-squared pairs cluttering the atomspace.
		;
		; Return the number of similarity values that were above the
		; cutoff.
		(define (batch-simlist ITEM ITEM-LIST)
			(for-each
				(lambda (item) (compute-sim ITEM item))
				ITEM-LIST)
		)

		; Loop over the entire list of items, and compute similarity
		; scores between pairs of them.  This might take a very long time!
		; Serial version, see also parallel version below.
		(define (batch-sim-pairs ITEM-LIST)

			(define len (length ITEM-LIST))
			(define tot (* 0.5 len (+ len 1))) ; number of pairs todo
			(define done 0)      ; items done
			(define prevcomp 0)  ; pairs computed
			(define elapsed-secs (make-elapsed-secs))

			(define (do-one-and-rpt ITM-LST)
				; Reverse, so that we work from the diagonal, outwards.
				(define rev-lst (reverse ITM-LST))
				(batch-simlist (car rev-lst) rev-lst)
				(set! done (+ done 1))
				(if (eqv? 0 (modulo done 10))
					(let* ((elapsed (- (elapsed-secs) 1.0e-6))
							(nprdone (* 0.5 done (+ done 1))) ; number of pairs done
							(rate (/ (- compcnt prevcomp) elapsed))
						)

						; Frac is the percentage fraction that had
						; similarities greater than the cutoff.
						; Rate is the rate of computing pairs, whether
						; or not they are actually saved.
						(format #t
							 "Done ~A/~A Frac=~,2f% Time: ~,1f Done: ~,1f% Rate=~,2f prs/sec (~,2f sec/pr)\n"
							done len
							(* 100.0 (/ savecnt nprdone))
							elapsed
							(* 100.0 (/ nprdone tot))
							rate
							(/ 1.0 rate)
						)
						(set! prevcomp compcnt)
				)))


			; Reset the stats, before restarting
			(set! compcnt 0)
			(set! savecnt 0)
			(for-each
				(lambda (n) (do-one-and-rpt (take ITEM-LIST n)))
				(iota (length ITEM-LIST) 1))
		)

		; Loop over the entire list of items, and compute similarity
		; scores for them.  Hacked parallel version.
		; XXX FIXME -- due to guile locking flakiness, setting NTHREADS
		; to any value bigger than 3 results in a net slow-down, and even
		; for NTHREADS=3, it can be pretty bad suckage, as the system
		; starts thrashing due to some kind of live-lock like thing.
		; The root cause for this is unclear. It may be thrashing on the
		; locks in the AtomSpace. The current guile bindings hit the
		; AtomSpace surprisingly often. See issue #2553 for a possible
		; solution.
		(define (para-batch-sim-pairs ITEM-LIST NTHREADS)

			(define len (length ITEM-LIST))
			(define tot (* 0.5 len (+ len 1)))
			(define done 0)
			(define prevcomp 0)  ; pairs computed
			(define elapsed-secs (make-elapsed-secs))

			(define (do-one-and-rpt ITM-LST)
				; Reverse, so that we work from the diagonal, outwards.
				(define rev-lst (reverse ITM-LST))
				(batch-simlist (car rev-lst) rev-lst)
				; These sets are not thread-safe but I don't care.
				; XXX this is easy to fix, just use atomic boxes.
				(set! done (+ done 1))
				(if (eqv? 0 (modulo done 20))
					(let* ((elapsed (- (elapsed-secs) 1.0e-6))
							(nprdone (* 0.5 done (+ done 1))) ; number of pairs done
							(rate (/ (- compcnt prevcomp) elapsed))
							)
						(format #t
							 "Done ~A/~A Frac=~,2f% Time: ~,1f Done: ~,1f% Rate=~,2f prs/sec (~,2f sec/pr)\n"
							done len
							(* 100.0 (/ (* NTHREADS savecnt) nprdone))
							elapsed
							(* 100.0 (/ nprdone tot))
							rate
							(/ 1.0 rate)
						)
						(set! prevcomp compcnt)
					)))

			; Reset the states, before restarting
			(set! compcnt 0)
			(set! savecnt 0)
			(for-each
				(lambda (thnum)
					(call-with-new-thread
						(lambda ()
							(for-each
								(lambda (n) (do-one-and-rpt (take ITEM-LIST n)))
								(iota (length ITEM-LIST) thnum NTHREADS)))))
				(iota NTHREADS 1))

			(format #t "Started ~d threads\n" NTHREADS)
		)

		; Get the entire basis, and sort it according to frequency.
		; The returned list has the most frequent basis elements
		; first in the list.
		(define (get-sorted-basis)
			(define basis (if MTM?
					(wldobj 'right-basis)
					(wldobj 'left-basis)))
			(define supp-obj (add-support-api wldobj))
			(define (nobs ITEM)
				(if MTM?
					(supp-obj 'left-count ITEM)
					(supp-obj 'right-count ITEM)))

			; Rank so that the commonest items are first in the list.
			(sort basis
				(lambda (ATOM-A ATOM-B) (> (nobs ATOM-A) (nobs ATOM-B))))
		)

		; Loop over the top-N most frequent basis elements
		(define (batch TOP-N)
			(define elapsed (make-elapsed-secs))
			(define sbase (get-sorted-basis))
			(define nbase (length sbase))
			; `take` fails if asked to take more than length of list.
			(define num (if (< TOP-N nbase) TOP-N nbase))
			(define top-items (take sbase num))
			(format #t "Obtained top ~A items in ~A secs\n" num (elapsed))
			(batch-sim-pairs top-items))

		; Loop over the top-N most frequent basis elements
		; XXX Due to guile flakiness, this works very poorly for
		; NTHREADS greater than 3, and even then it's prone to
		; terrible slowdowns.
		(define (para-batch TOP-N NTHREADS)
			(define top-items (take (get-sorted-basis) TOP-N))
			(para-batch-sim-pairs top-items NTHREADS))

		; Methods on this class.
		(lambda (message . args)
			(case message

				((compute-similarity)  (apply compute-sim args))
				((batch-compute)       (apply batch args))
				((parallel-batch)      (apply para-batch args))
				((batch-list)          (apply batch-sim-pairs args))

				(else                  (apply LLOBJ (cons message args)))
		)))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
