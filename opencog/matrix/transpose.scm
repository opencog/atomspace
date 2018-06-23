;
; transpose.scm
;
; Define object-oriented class API's for computing the counts,
; frequencies and entropies of of a matrix times it's transpose.
;
; Copyright (c) 2017, 2018 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; A matrix times it's transpose is again a matrix. Actually, there are
; two: M^TM and MM^T. One may be interested in a number of different
; properties of these two, includings support, frequencies, entropies,
; mutual information, etc. It is more computationally efficient to
; re-order the order in which the marginals for these two matrices are
; computed: that is what is done below.
;
; If it were not for these efficiencies, one could instead just write
; an api object that took the product of two matricies.  Conceptually,
; that would be simpler. And a lot slower.  Thus, this object.
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

#! xxxxxxxxxxxxxxxxxxxxxxx
(define*-public (add-transpose-api LLOBJ
	 #:optional (ID (LLOBJ 'id)))
"
  add-support-api LLOBJ ID - Extend LLOBJ with methods to retreive
  marginals (wild-card sums) for
xxxxxxxxxx
  support, size and length subtotals on rows and columns. The values
  are retreived from the \"margins\", attached to the matrix wild-cards.
  This class assumes the marginals were previously computed and
  attached to the wildcards.

  The margins (the pre-computed values) can be populated by saying
  `((add-support-compute LLOBJ) 'cache-all)`
  The `add-support-api` and `add-support-compute` API's are designed
  to work together and complement one-another.

  Optional argument ID is #f to use the default value key;
  otherwise a filtered key is used.
"
	; ----------------------------------------------------
	; Key under which the matrix l_p norms are stored.
	(define key-name
		(if (and ID (LLOBJ 'filters?))
			(string-append "*-Norm Key " ID)
			"*-Norm Key-*"))

	(define norm-key (PredicateNode key-name))

	(define (set-norms ATOM L0 L1 L2)
		(cog-set-value! ATOM norm-key (FloatValue L0 L1 L2)))

	; User might ask for something not in the matrix. In that
	; case, cog-value-ref will throw 'wrong-type-arg. If this
	; happens, just return zero.
	(define (get-support ATOM)
		(catch 'wrong-type-arg
			(lambda () (cog-value-ref (cog-value ATOM norm-key) 0))
			(lambda (key . args) 0)))

	(define (get-count ATOM)
		(catch 'wrong-type-arg
			(lambda () (cog-value-ref (cog-value ATOM norm-key) 1))
			(lambda (key . args) 0)))

	(define (get-length ATOM)
		(catch 'wrong-type-arg
			(lambda () (cog-value-ref (cog-value ATOM norm-key) 2))
			(lambda (key . args) 0)))

	;--------
	(define (get-left-support ITEM)
		(get-support (LLOBJ 'left-wildcard ITEM)))

	(define (get-left-count ITEM)
		(get-count (LLOBJ 'left-wildcard ITEM)))

	(define (get-left-length ITEM)
		(get-length (LLOBJ 'left-wildcard ITEM)))

	(define (set-left-norms ITEM L0 L1 L2)
		(set-norms (LLOBJ 'left-wildcard ITEM) L0 L1 L2))

	;--------
	(define (get-right-support ITEM)
		(get-support (LLOBJ 'right-wildcard ITEM)))

	(define (get-right-count ITEM)
		(get-count (LLOBJ 'right-wildcard ITEM)))

	(define (get-right-length ITEM)
		(get-length (LLOBJ 'right-wildcard ITEM)))

	(define (set-right-norms ITEM L0 L1 L2)
		(set-norms (LLOBJ 'right-wildcard ITEM) L0 L1 L2))

	;--------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((left-support)       (apply get-left-support args))
			((right-support)      (apply get-right-support args))
			((left-count)         (apply get-left-count args))
			((right-count)        (apply get-right-count args))
			((left-length)        (apply get-left-length args))
			((right-length)       (apply get-right-length args))
			((set-left-norms)     (apply set-left-norms args))
			((set-right-norms)    (apply set-right-norms args))
			(else                 (apply LLOBJ (cons message args)))))
)

xxxxxxxxxxxxxxxxxxxx !#
; ---------------------------------------------------------------------

(define*-public (add-transpose-compute LLOBJ
	 #:optional (GET-CNT 'get-count))
"
  add-transpose-compute LLOBJ - Extend LLOBJ with methods to compute
  marginals (wild-card sums) for a matrix times it's transpose. These
  include the support (lp-norm for p=0), the count (lp-norm for p=1),
  the Euclidean length (lp-norm for p=2) and the general lp-norm.

  This is very much like what the support-api does, except that these
  compute the marginals for the matrixes MM^T and M^TM. It is more
  efficient to compute support here, than it is to compute the product
  first, and only then slap the support API on top.

  By default, the counts and cached marginals are used to perform the
  computation. This can be over-ridden by specifying optional methods
  to access the desired numbers and marginals.  Note that the marginals
  MUST have been pre-computed, else the calculations here will fail.
xxxxxxxxxx
  These all work with the counts for the
  pairs, and NOT the frequencies!  None of these use any pre-computed
  (marginal, or \"cached\") values; instead, they compute the norms from
  the raw matrix data.  The computed norms are not places in the
  margins or cached or saved (unless the 'cache-all method is invoked.)

  The 'cache-all method computes norms for the ENTIRE matrix, and
  places them in the margins, i.e. as values on the wild-cards of the
  matrix.  This can take a lot of CPU-time. After the 'cache-all
  method has been invoked, the `(add-support-api)` object can be
  used to return these values.

  In order for 'cache-all to work, the full matrix must avaialble
  in RAM.  It can be fetched by calling `(LLOBJ 'fetch-pairs)`.
  After computing the marginals, it is wise to store them back to
  disk. This can be done with `((make-store LLOBJ) 'sotre-wildcards)`

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  The left-support-set consists of all pairs (x,y), for fixed y, for
  which N(x,y) > 0. The right-support-set is the same, for fixed x.

  The support is the size of the support-set.  AKA the l_0 norm.
  The left-support is the number of non-zero entries in a column.

  The left-count is the wild-card sum_x N(x,y) for fixed y.
  That is, for a given column y, this sums all counts in that column.

  The left-length is sqrt(sum_x N^2(x,y)) for fixed y.

  The left-lp-norm is |sum_x N^p(x,y)|^1/p for fixed y.

  The total-support is sum_x sum_y 1
  That is, the total number of non-zero entries in the matrix.

  The total-count is N(*,*) = sum_x sum_y N(x,y)
  That is, the total of all count entries in the matrix.

  Here, the LLOBJ is expected to be an object, with valid counts
  associated with each pair. LLOBJ is expected to have working,
  functional methods for 'left-type and 'right-type on it.

  By default, the N(x,y) is taken to be the 'get-count method on LLOBJ,
  i.e. it is literally the count. The optional argument GET-CNT allows
  this to be over-ridden with any other method that returns a number.
  For example, to compute the lengths and norms for frequencies, simply
  pass 'pair-freq as the second argument: Any method that takes a pair
  and returns a number is allowed.
"
	(let ((star-obj (add-pair-stars LLOBJ))
			(api-obj (add-support-api LLOBJ))
			(get-cnt (lambda (x) (LLOBJ GET-CNT x)))
		)

		; -------------
		; Filter and return only pairs with non-zero count.
		; Internal use only.
		(define (non-zero-filter LIST)
			(filter (lambda (lopr) (< 0 (get-cnt lopr))) LIST))

		; Return a list of all pairs (x, y) for y == ITEM for which
		; N(x,y) > 0.  Specifically, this returns the pairs which
		; are holding the counts (and not the low-level pairs).
		(define (get-left-support-set ITEM)
			(non-zero-filter (star-obj 'left-stars ITEM)))

		; Same as above, but on the right.
		(define (get-right-support-set ITEM)
			(non-zero-filter (star-obj 'right-stars ITEM)))

		; -------------
		; Return how many non-zero items are in the list.
		(define (get-support-size LIST)
			(fold
				(lambda (lopr sum)
					(if (< 0 (get-cnt lopr)) (+ sum 1) sum))
				0
				LIST))

		; Should return a value exactly equal to
		; (length (get-left-support ITEM))
		; Equivalently to the l_0 norm (l_p norm for p=0)
		(define (get-left-support-size ITEM)
			(get-support-size (star-obj 'left-stars ITEM)))

		(define (get-right-support-size ITEM)
			(get-support-size (star-obj 'right-stars ITEM)))

		; -------------
		; Return the sum of the counts on the list
		(define (sum-count LIST)
			(fold
				(lambda (lopr sum) (+ sum (get-cnt lopr)))
				0
				LIST))

		; Should return a value exactly equal to 'left-count
		; Equivalently to the l_1 norm (l_p norm for p=1)
		(define (sum-left-count ITEM)
			(sum-count (star-obj 'left-stars ITEM)))

		(define (sum-right-count ITEM)
			(sum-count (star-obj 'right-stars ITEM)))

		; -------------
		; Return the Euclidean length of the list
		(define (sum-length LIST)
			(define tot
				(fold
					(lambda (lopr sum)
						(define cnt (get-cnt lopr))
						(+ sum (* cnt cnt)))
					0
					LIST))
			(sqrt tot))

		; Returns the Euclidean length aka the l_2 norm (l_p norm for p=2)
		(define (sum-left-length ITEM)
			(sum-length (star-obj 'left-stars ITEM)))

		(define (sum-right-length ITEM)
			(sum-length (star-obj 'right-stars ITEM)))

		; -------------
		; Return the lp-norm (Banach-space norm) of the counts
		; on LIST.  Viz sum_k N^p(k) for counted-pairs k in the
		; list
		(define (sum-lp-norm P LIST)
			(define tot
				(fold
					(lambda (lopr sum)
						(define cnt (get-cnt lopr))
						(+ sum (expt cnt P)))
					0
					LIST))
			(expt tot (/ 1.0 P)))

		(define (sum-left-lp-norm P ITEM)
			(sum-lp-norm P (star-obj 'left-stars ITEM)))

		(define (sum-right-lp-norm P ITEM)
			(sum-lp-norm P (star-obj 'right-stars ITEM)))

		; -------------
		; Compute grand-totals for the whole matrix.
		; These are computed from the left; there is an equivalent
		; computation from the right that should give exactly the same
		; results. We could/should be not lazy and double-check these
		; results in this way.
		(define (compute-total-support)
			(fold
				(lambda (item sum) (+ sum (get-right-support-size item)))
				0
				(star-obj 'left-basis)))

		(define (compute-total-count)
			(fold
				(lambda (item sum) (+ sum (sum-right-count item)))
				0
				(star-obj 'left-basis)))

		; -------------
		; Compute all l_0, l_1 and l_2 norms, attach them to the
		; wildcards, where the support-api can find them.

		(define start-time 0)
		(define (elapsed-secs)
			(define diff (- (current-time) start-time))
			(set! start-time (current-time))
			diff)

		; XXX FIXME can make this 3x faster by performing all three loops
		; at the same time.
		(define (left-marginals)
			(elapsed-secs)
			(for-each
				(lambda (ITEM)
					(define l0 (get-left-support-size ITEM))
					(define l1 (sum-left-count ITEM))
					(define l2 (sum-left-length ITEM))
					(api-obj 'set-left-norms ITEM l0 l1 l2))
				(star-obj 'right-basis))

			(format #t "Finished left norm marginals in ~A secs\n"
				(elapsed-secs)))

		(define (right-marginals)
			(elapsed-secs)
			(for-each
				(lambda (ITEM)
					(define l0 (get-right-support-size ITEM))
					(define l1 (sum-right-count ITEM))
					(define l2 (sum-right-length ITEM))
					(api-obj 'set-right-norms ITEM l0 l1 l2))
				(star-obj 'left-basis))
			(format #t "Finished right norm marginals in ~A secs\n"
				(elapsed-secs)))

		; Do both at once
		(define (cache-all)
			(left-marginals)
			(right-marginals))

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-support-set)   (apply get-left-support-set args))
				((right-support-set)  (apply get-right-support-set args))
				((left-support)       (apply get-left-support-size args))
				((right-support)      (apply get-right-support-size args))
				((left-count)         (apply sum-left-count args))
				((right-count)        (apply sum-right-count args))
				((left-length)        (apply sum-left-length args))
				((right-length)       (apply sum-right-length args))
				((left-lp-norm)       (apply sum-left-lp-norm args))
				((right-lp-norm)      (apply sum-right-lp-norm args))

				((total-support)      (compute-total-support))
				((total-count)        (compute-total-count))

				((left-marginals)     (left-marginals))
				((right-marginals)    (right-marginals))
				((cache-all)          (cache-all))

; XXX hack alert. We need something more elegant!?
; the language-learning clustering code uses this
; to invalidate the star objects in use.
				((clobber)            (star-obj 'clobber))
				(else                 (apply LLOBJ (cons message args))))
			)))

; ---------------------------------------------------------------------
