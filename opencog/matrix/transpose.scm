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
; properties of these two, including support, frequencies, entropies,
; mutual information, etc.
;
; One could, of course, define a generic matrix product, apply it to
; the matrix, and then use the generic support, frequency, entropy
; API's on this product matrix. Unfortunately, that would be
; computationally very slow and inefficient.  Thus the objects here,
; which are reasonably fast, by leveraging previously computed
; marginals.
;
; Note, by the way, that both of the above are symmetric matrices:
; that is, (M^TM)^T = M^TM and likewise for the other.  Thus, there
; are no left-right differences between the two.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define*-public (add-transpose-api LLOBJ
	 #:optional (ID (LLOBJ 'id)))
"
  add-transpose-api LLOBJ ID - Extend LLOBJ with methods to retrieve
  marginals (wild-card sums) for the support and count for the
  matrix-products M^TM and MM^T of the matrix M==LLOBJ times itself.

  The values are retrieved from the \"margins\", attached to the matrix
  wild-cards.  This class assumes the marginals were previously computed
  and attached to the wildcards.

  The margins (the pre-computed values) can be populated by saying
  `((add-transpose-compute LLOBJ) 'cache-all)`
  The `add-transpose-api` and `add-transpose-compute` API's are designed
  to work together and complement one-another. See the documentation on
  `add-transpose-compute` for detailed documentation.

  Optional argument ID is #f to use the default value key; otherwise
  a filtered key is used. That is, the marginals are fetched from a
  default location; however, that location can be changed by specifying
  it with the optional ID argument.
"
	; ----------------------------------------------------
	; Key under which the transpose-products norms are stored.
	(define mmt-name
		(if (and ID (LLOBJ 'filters?))
			(string-append "*-MM^T Product Key " ID "-*")
			"*-MM^T Product Key-*"))

	(define mtm-name
		(if (and ID (LLOBJ 'filters?))
			(string-append "*-M^TM Product Key " ID "-*")
			"*-M^TM Product Key-*"))

	(define mmt-key (PredicateNode mmt-name))
	(define mtm-key (PredicateNode mtm-name))

	(define (set-norms KEY ATOM L0 L1 L2 LQ)
		(cog-set-value! ATOM KEY (FloatValue L0 L1 L2 LQ)))

	; User might ask for something not in the matrix. In that
	; case, cog-value-ref will throw 'wrong-type-arg. If this
	; happens, just return zero.
	(define (get-support KEY ATOM)
		(catch 'wrong-type-arg
			(lambda () (cog-value-ref (cog-value ATOM KEY) 0))
			(lambda (key . args) 0)))

	(define (get-count KEY ATOM)
		(catch 'wrong-type-arg
			(lambda () (cog-value-ref (cog-value ATOM KEY) 1))
			(lambda (key . args) 0)))

	(define (get-length KEY ATOM)
		(catch 'wrong-type-arg
			(lambda () (cog-value-ref (cog-value ATOM KEY) 2))
			(lambda (key . args) 0)))

	(define (get-amplitude KEY ATOM)
		(catch 'wrong-type-arg
			(lambda () (cog-value-ref (cog-value ATOM KEY) 3))
			(lambda (key . args) 0)))

	;--------
	; The internal sum is over the left items, so hang on the left.
	; This is an arbitrary choice, but seems less confusing than the
	; other one.
	(define (get-mtm-support ITEM)
		(get-support mtm-key (LLOBJ 'left-wildcard ITEM)))

	(define (get-mtm-count ITEM)
		(get-count mtm-key (LLOBJ 'left-wildcard ITEM)))

	(define (get-mtm-length ITEM)
		(get-length mtm-key (LLOBJ 'left-wildcard ITEM)))

	(define (get-mtm-amplitude ITEM)
		(get-amplitude mtm-key (LLOBJ 'left-wildcard ITEM)))

	(define (set-mtm-norms ITEM L0 L1 L2 LQ)
		(set-norms mtm-key (LLOBJ 'left-wildcard ITEM) L0 L1 L2 LQ))

	;--------
	(define (get-mmt-support ITEM)
		(get-support mmt-key (LLOBJ 'right-wildcard ITEM)))

	(define (get-mmt-count ITEM)
		(get-count mmt-key (LLOBJ 'right-wildcard ITEM)))

	(define (get-mmt-length ITEM)
		(get-length mmt-key (LLOBJ 'right-wildcard ITEM)))

	(define (get-mmt-amplitude ITEM)
		(get-amplitude mmt-key (LLOBJ 'right-wildcard ITEM)))

	(define (set-mmt-norms ITEM L0 L1 L2 LQ)
		(set-norms mmt-key (LLOBJ 'right-wildcard ITEM) L0 L1 L2 LQ))

	;--------
	(define (tot-mmt-support)
		(get-support mmt-key (LLOBJ 'wild-wild)))

	(define (tot-mmt-count)
		(get-count mmt-key (LLOBJ 'wild-wild)))

	(define (tot-mmt-length)
		(get-length mmt-key (LLOBJ 'wild-wild)))

	(define (tot-mmt-amplitude)
		(get-amplitude mmt-key (LLOBJ 'wild-wild)))

	(define (set-mmt-totals L0 L1 L2 LQ)
		(set-norms mmt-key (LLOBJ 'wild-wild) L0 L1 L2 LQ))

	;--------
	(define (tot-mtm-support)
		(get-support mtm-key (LLOBJ 'wild-wild)))

	(define (tot-mtm-count)
		(get-count mtm-key (LLOBJ 'wild-wild)))

	(define (tot-mtm-length)
		(get-length mtm-key (LLOBJ 'wild-wild)))

	(define (tot-mtm-amplitude)
		(get-amplitude mtm-key (LLOBJ 'wild-wild)))

	(define (set-mtm-totals L0 L1 L2 LQ)
		(set-norms mtm-key (LLOBJ 'wild-wild) L0 L1 L2 LQ))

	;--------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((mtm-support)        (apply get-mtm-support args))
			((mmt-support)        (apply get-mmt-support args))
			((mtm-count)          (apply get-mtm-count args))
			((mmt-count)          (apply get-mmt-count args))
			((mtm-length)         (apply get-mtm-length args))
			((mmt-length)         (apply get-mmt-length args))
			((mtm-amplitude)      (apply get-mtm-amplitude args))
			((mmt-amplitude)      (apply get-mmt-amplitude args))
			((set-mtm-norms)      (apply set-mtm-norms args))
			((set-mmt-norms)      (apply set-mmt-norms args))

			((total-mtm-support)  (tot-mtm-support))
			((total-mmt-support)  (tot-mmt-support))
			((total-mtm-count)    (tot-mtm-count))
			((total-mmt-count)    (tot-mmt-count))
			((total-mtm-length)   (tot-mtm-length))
			((total-mmt-length)   (tot-mmt-length))
			((total-mtm-amplitude) (tot-mtm-amplitude))
			((total-mmt-amplitude) (tot-mmt-amplitude))
			((set-mtm-totals)     (apply set-mtm-totals args))
			((set-mmt-totals)     (apply set-mmt-totals args))
			(else                 (apply LLOBJ (cons message args)))))
)

; ---------------------------------------------------------------------

(define*-public (add-transpose-compute LLOBJ #:key
	 (GET-COUNT 'get-count)
	 (LEFT-SUPPORT    'left-support)
	 (RIGHT-SUPPORT   'right-support)
	 (LEFT-COUNT      'left-count)
	 (RIGHT-COUNT     'right-count)
	 (LEFT-LENGTH     'left-length)
	 (RIGHT-LENGTH    'right-length)
	 (LEFT-AMPLITUDE  'left-amplitude)
	 (RIGHT-AMPLITUDE 'right-amplitude))
"
  add-transpose-compute LLOBJ - Extend LLOBJ with methods to compute
  marginals (wild-card sums) for a matrix times it's transpose. These
  include the support (lp-norm for p=0), the amplitude (lp-norm for
  p=0.5), the count (lp-norm for p=1) and the Euclidean length (lp-norm
  for p=2).

  This is very much like what the support-api does, except that these
  compute the marginals for the matrixes MM^T and M^TM. It is more
  efficient to compute support here, than it is to compute the product
  first, and only then slap the support API on top.

  By default, the counts and cached marginals are used to perform the
  computation. This can be over-ridden by specifying optional methods
  to access the desired numbers and marginals.  Note that the marginals
  MUST have been pre-computed, else the calculations here will fail.
  The computed norms are not placed back into the atomspace after being
  computed (unless the 'cache-all method is invoked.)

  The 'cache-all method computes all of the various norms, and then
  places them in the margins, i.e. as values on the wild-cards on the
  matrix.  This can take a lot of CPU-time. After the 'cache-all
  method has been invoked, the `(add-transpose-api)` object can be
  used to return these values.

  In order for 'cache-all to work, the full matrix must available
  in RAM (and the marginals on it must have been pre-computed).  The
  matrix can be fetched by calling `(LLOBJ 'fetch-pairs)`. After
  'cache-all has done it's batch computation, it is wise to store the
  results to disk. Do this with `((make-store LLOBJ) 'store-wildcards)`

  Most typical applications do not need both the M^TM and the MM^T
  products. It is more efficient computationally, and storage-wise,
  to only compute the side that is needed. For this, use either
  'all-mtm-marginals or 'all-mmt-marginals in place of 'cache-all.

  Some applications need to recompute individual rows or columns
  because of some manipulation applied to M. Recomputation (and caching)
  can be done with the 'set-mtm-marginals and 'set-mmt-marginals
  methods. They take one argument: the column or row to be recomputed.

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  Let D(x,y) == 1 if N(x,y) > 0; otherwise D(x,y) == 0.
  Wild-cards are as usual:  e.g. D(x,*) = sum_y D(x,y), etc.

  Note that M^TM is symmetric, that is, (M^TM)^T = M^TM and so the
  rows are the same as the columns in this matrix. Thus, the left and
  right sums are identical, and cannot be distinguished.  Likewise,
  the left and right marginals are identical.

  Of course, MM^T is also symmetric. Thus, this computes two sets of
  norms: those for M^TM and those for MM^T. All methods are likewise
  labelled.

  The 'mtm-support is equal to sum_x D(x,y) D(x,*)
  The 'mtm-support is the number of non-zero entries in a column or row
  of the matrix M^TM. It is the l_0 norm of columns or rows in M^TM.

  The 'mmt-support is equal to sum_y D(x,y) D(*,y)

  The 'mtm-count is the wild-card sum_x N(x,y) N(x,*) for fixed y.
  The 'mmt-count is the wild-card sum_y N(x,y) N(*,y) for fixed x.

  The 'mtm-length is the wild-card sum_x N^2(x,y) N^2(x,*) for fixed y.
  The 'mmt-length is the wild-card sum_y N^2(x,y) N^2(*,y) for fixed x.

  The 'mtm-amplitude is the wild-card sum_x sqrt(N(x,y) N(x,*)) for fixed y.
  The 'mmt-amplitude is the wild-card sum_y sqrt(N(x,y) N(*,y)) for fixed x.

  The 'total-mtm-support is sum_x D(x,*) D(x,*)
  The 'total-mmt-support is sum_y D(*,y) D(*,y)

  The 'total-mtm-count is sum_x N(x,*) N(x,*)
  The 'total-mmt-count is sum_y N(*,y) N(*,y)

  The 'total-mtm-length is sum_x N^2(x,*) N^2(x,*)
  The 'total-mmt-length is sum_y N^2(*,y) N^2(*,y)

  The 'total-mtm-amplitude is sum_x sqrt(N(x,*) N(x,*))
  The 'total-mmt-amplitude is sum_y sqrt(N(*,y) N(*,y))

  Here, the LLOBJ is expected to be an object, with valid counts
  and count-marginals.

  By default, the N(x,y) is taken to be the 'get-count method on LLOBJ,
  i.e. it is literally the count. It can be over-ridden with the
  #:GET-COUNT key argument. For example, to use frequencies instead of
  counts, say `(add-transpose-compute LLOBJ #:GET-COUNT 'get-freq)`,
  where LLOBJ is assumed to have a 'get-freq method that returns a
  number that is used instead of the count.

  The other marginals default as follows, and are over-ridden as:
  D(*,y) == 'left-support    override with #:LEFT-SUPPORT
  D(x,*) == 'right-support   override with #:RIGHT-SUPPORT
  N(*,y) == 'left-count      override with #:LEFT-COUNT
  N(x,*) == 'right-count     override with #:RIGHT-COUNT
  sqrt(N^2(*,y)) == 'left-length   override with #:LEFT-LENGTH
  sqrt(N^2(x,*)) == 'right-length  override with #:RIGHT-LENGTH
  (N^0.5(*,y))^2 == 'left-amplitude   override with #:LEFT-AMPLITUDE
  (N^0.5(x,*))^2 == 'right-amplitude  override with #:RIGHT-AMPLITUDE
"

	(let* ((star-obj     (add-pair-stars LLOBJ))
			(support-obj   (add-support-api LLOBJ))
			(api-obj       (add-transpose-api LLOBJ))
			(get-cnt       (lambda (x) (LLOBJ GET-COUNT x)))
			(get-pr-cnt    (lambda (l r) (get-cnt (LLOBJ 'get-pair l r))))
			(left-support  (lambda (x) (support-obj LEFT-SUPPORT x)))
			(right-support (lambda (x) (support-obj RIGHT-SUPPORT x)))
			(left-count    (lambda (x) (support-obj LEFT-COUNT x)))
			(right-count   (lambda (x) (support-obj RIGHT-COUNT x)))
			(left-length   (lambda (x) (support-obj LEFT-LENGTH x)))
			(right-length  (lambda (x) (support-obj RIGHT-LENGTH x)))
			(left-amplitude  (lambda (x) (support-obj LEFT-AMPLITUDE x)))
			(right-amplitude (lambda (x) (support-obj RIGHT-AMPLITUDE x)))
		)

		; -------------
		; Return a list of all pairs (x,y) for y == ITEM for which
		; sum_x N(x,*) N(x,y) > 0.
		(define (get-mtm-support-set ITEM)
			(filter (lambda (ldual)
				(< 0 (* (right-count ldual) (get-pr-cnt ldual ITEM))))
			(star-obj 'left-duals ITEM)))

		; Same as above, but on the right.
		(define (get-mmt-support-set ITEM)
			(filter (lambda (rdual)
				(< 0 (* (left-count rdual) (get-pr-cnt ITEM rdual))))
			(star-obj 'right-duals ITEM)))

		; -------------
		; Return how many non-zero items are in the list.
		; Identical to (length (get-mmt-support-set ITEM))
		(define (sum-mmt-support ITEM)
			(fold (lambda (rdual sum)
				(if (< 0 (get-pr-cnt ITEM rdual))
					(+ sum (left-support rdual))
					sum))
				0 (star-obj 'right-duals ITEM)))

		(define (sum-mtm-support ITEM)
			(fold (lambda (ldual sum)
				(if (< 0 (get-pr-cnt ldual ITEM))
					(+ sum (right-support ldual))
					sum))
				0 (star-obj 'left-duals ITEM)))

		; -------------
		; Sum counts
		(define (sum-mmt-count ITEM)
			(fold (lambda (rdual sum)
				(+ sum (* (left-count rdual) (get-pr-cnt ITEM rdual)))) 0
				(star-obj 'right-duals ITEM)))

		(define (sum-mtm-count ITEM)
			(fold (lambda (ldual sum)
				(+ sum (* (right-count ldual) (get-pr-cnt ldual ITEM)))) 0
				(star-obj 'left-duals ITEM)))

		; -------------
		; Sum lengths
		(define (sum-mmt-length ITEM)
			(sqrt (fold (lambda (rdual sum)
					(define cnt (get-pr-cnt ITEM rdual))
					(define rms (left-length rdual))
					(+ sum (* rms rms cnt cnt )))
				0 (star-obj 'right-duals ITEM))))

		(define (sum-mtm-length ITEM)
			(sqrt (fold (lambda (ldual sum)
					(define cnt (get-pr-cnt ldual ITEM))
					(define rms (right-length ldual))
					(+ sum (* rms rms cnt cnt)))
				0 (star-obj 'left-duals ITEM))))

		; -------------
		; Sum amplitudes
		(define (sum-mmt-amplitude ITEM)
			(define amp
				(fold (lambda (rdual sum)
						(define cnt (get-pr-cnt ITEM rdual))
						(+ sum (sqrt (* (left-amplitude rdual) cnt ))))
					0 (star-obj 'right-duals ITEM)))
			(* amp amp))

		(define (sum-mtm-amplitude ITEM)
			(define amp
				(fold (lambda (ldual sum)
						(define cnt (get-pr-cnt ldual ITEM))
						(+ sum (sqrt (* (right-amplitude ldual) cnt ))))
					0 (star-obj 'left-duals ITEM)))
			(* amp amp))

		; -------------
		; Compute grand-totals for the two matrix products.
		(define (compute-total-mtm-support)
			(fold
				(lambda (item sum) (+ sum (api-obj 'mtm-support item))) 0
				(star-obj 'right-basis)))

		(define (compute-total-mtm-count)
			(fold
				(lambda (item sum) (+ sum (api-obj 'mtm-count item))) 0
				(star-obj 'right-basis)))

		(define (compute-total-mtm-length)
			; The mtm-length returns the RMS, so we square it before
			; summing. Then take the root again, at the end.
			(sqrt (fold
				(lambda (item sum)
					(define rms (api-obj 'mtm-length item))
					(+ sum (* rms rms)))
				0 (star-obj 'right-basis))))

		(define (compute-total-mtm-amplitude)
			(define amp
				(fold
					(lambda (item sum) (+ sum (sqrt (api-obj 'mtm-amplitude item))))
					0 (star-obj 'right-basis)))
			(* amp amp))

		(define (compute-total-mmt-support)
			(fold
				(lambda (item sum) (+ sum (api-obj 'mmt-support item))) 0
				(star-obj 'left-basis)))

		(define (compute-total-mmt-count)
			(fold
				(lambda (item sum) (+ sum (api-obj 'mmt-count item))) 0
				(star-obj 'left-basis)))

		(define (compute-total-mmt-length)
			; The mmt-length returns the RMS, so we square it before
			; summing. Then take the root again, at the end.
			(sqrt (fold
				(lambda (item sum)
					(define rms (api-obj 'mmt-length item))
					(+ sum (* rms rms)))
				0 (star-obj 'left-basis))))

		(define (compute-total-mmt-amplitude)
			(define amp
				(fold
					(lambda (item sum) (+ sum (sqrt (api-obj 'mmt-amplitude item))))
					0 (star-obj 'left-basis)))
			(* amp amp))

		; -------------
		; Compute all l_0, l_0.5, l_1 and l_2 norms, attach them to the
		; wildcards, where the transpose-api can find them.

		; Equivalent to
		;    (define l0 (sum-mtm-support ITEM))
		;    (define l1 (sum-mtm-count ITEM))
		;    (define l2 (sum-mtm-length ITEM))
		;    (define lq (sum-mtm-amplitude ITEM))
		; but 4x faster by performing all four loops at the
		; same time.
		(define (set-mtm-marginals ITEM)
			(define l0 0.0)
			(define l1 0.0)
			(define l2 0.0)
			(define lq 0.0)
			(for-each
				(lambda (ldual)
					(define cnt (get-pr-cnt ldual ITEM))
					(define sup (if (< 0 cnt) (right-support ldual) 0))
					(define term (* (right-count ldual) cnt))
					(define rms (* (right-length ldual) cnt))
					(define len (* rms rms))
					(define amp (sqrt (* (right-amplitude ldual) cnt)))

					(set! l0 (+ l0 sup))
					(set! l1 (+ l1 term))
					(set! l2 (+ l2 len))
					(set! lq (+ lq amp))
				)
				(star-obj 'left-duals ITEM))
			(api-obj 'set-mtm-norms ITEM l0 l1 l2 lq))

		(define (set-mmt-marginals ITEM)
			(define l0 0.0)
			(define l1 0.0)
			(define l2 0.0)
			(define lq 0.0)
			(for-each
				(lambda (rdual)
					(define cnt (get-pr-cnt ITEM rdual))
					(define sup (if (< 0 cnt) (left-support rdual) 0))
					(define term (* (left-count rdual) cnt))
					(define rms (* (left-length rdual) cnt))
					(define len (* rms rms))
					(define amp (sqrt (* (left-amplitude rdual) cnt)))

					(set! l0 (+ l0 sup))
					(set! l1 (+ l1 term))
					(set! l2 (+ l2 len))
					(set! lq (+ lq amp))
				)
				(star-obj 'right-duals ITEM))
			(api-obj 'set-mmt-norms ITEM l0 l1 l2 lq))

		; Compute the grand-totals
		(define (set-mtm-totals)
			(let ((mtm-sup (compute-total-mtm-support))
					(mtm-cnt (compute-total-mtm-count))
					(mtm-len (compute-total-mtm-length))
					(mtm-amp (compute-total-mtm-amplitude)))
				(api-obj 'set-mtm-totals mtm-sup mtm-cnt mtm-len mtm-amp)))

		(define (all-mtm-marginals)
			(define elapsed-secs (make-elapsed-secs))

			; Loop over each item in the right basis ...
			(for-each set-mtm-marginals (star-obj 'right-basis))
			(format #t "Finished mtm norm marginals in ~A secs\n"
				(elapsed-secs))

			(set-mtm-totals)  ;; Compute the grand-totals
			(format #t "Finished mtm totals in ~A secs\n"
				(elapsed-secs)))

		; Compute the grand-totals
		(define (set-mmt-totals)
			(let ((mmt-sup (compute-total-mmt-support))
					(mmt-cnt (compute-total-mmt-count))
					(mmt-len (compute-total-mmt-length))
					(mmt-amp (compute-total-mmt-amplitude)))
				(api-obj 'set-mmt-totals mmt-sup mmt-cnt mmt-len mmt-amp)))

		(define (all-mmt-marginals)
			(define elapsed-secs (make-elapsed-secs))

			; Loop over each item in the left basis ...
			(for-each set-mmt-marginals (star-obj 'left-basis))
			(format #t "Finished mmt norm marginals in ~A secs\n"
				(elapsed-secs))

			(set-mmt-totals)  ;; Compute the grand-totals
			(format #t "Finished mmt totals in ~A secs\n"
				(elapsed-secs)))

		; Do both at once
		(define (cache-all)
			(all-mmt-marginals)
			(all-mtm-marginals))

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((mtm-support-set)    (apply get-mtm-support-set args))
				((mmt-support-set)    (apply get-mmt-support-set args))
				((mtm-support)        (apply sum-mtm-support args))
				((mmt-support)        (apply sum-mmt-support args))
				((mtm-count)          (apply sum-mtm-count args))
				((mmt-count)          (apply sum-mmt-count args))
				((mtm-length)         (apply sum-mtm-length args))
				((mmt-length)         (apply sum-mmt-length args))
				((mtm-amplitude)      (apply sum-mtm-amplitude args))
				((mmt-amplitude)      (apply sum-mmt-amplitude args))

				((total-mtm-support)  (compute-total-mtm-support))
				((total-mtm-count)    (compute-total-mtm-count))
				((total-mtm-length)   (compute-total-mtm-length))
				((total-mtm-amplitude) (compute-total-mtm-amplitude))
				((total-mmt-support)  (compute-total-mmt-support))
				((total-mmt-count)    (compute-total-mmt-count))
				((total-mmt-length)   (compute-total-mmt-length))
				((total-mmt-amplitude) (compute-total-mmt-amplitude))

				((set-mtm-marginals)  (apply set-mtm-marginals args))
				((set-mmt-marginals)  (apply set-mmt-marginals args))
				((set-mtm-totals)     (apply set-mtm-totals args))
				((set-mmt-totals)     (apply set-mmt-totals args))

				((all-mtm-marginals)  (all-mtm-marginals))
				((all-mmt-marginals)  (all-mmt-marginals))
				((cache-all)          (cache-all))

				((clobber)            (star-obj 'clobber))
				(else                 (apply LLOBJ (cons message args))))
			)))

; ---------------------------------------------------------------------
; Example usage
; This works great, for the pseudo-csets obtained from disjunct
; counting.  The exposed indexes are then word-nodes, and the internal
; sums run over the disjuncts. Viz: this matrix is N(word,disjunct)
; so that N(*,d) == 'left-count and
; 'mmt-count (WordNode "w") is sum_d N(w,d) N(*,d)
;
; (define psa (pseudo-cset-api))
; (define tcc (add-transpose-compute psa))
; (tcc 'mmt-count (Word "eyes"))
;
; Results for the "en_mtwo" dataset:
; (tcc 'mmt-count (Word "eyes"))   ; 29484348.0
; (tcc 'mmt-count (Word "want"))   ; 23512273.0
; (tcc 'mmt-count (Word "nice"))   ; 2653722.0
;
; And again, for frequencies:
; (define pfa (add-pair-freq-api psa #:nothrow #t))
; (define tcf (add-transpose-compute pfa
;                 #:GET-COUNT 'pair-freq #:LEFT-COUNT 'left-wild-freq))
;
; (tcf 'mmt-count (Word "eyes"))   ; 8.6245e-8
; (tcf 'mmt-count (Word "want"))   ; 6.8776e-8
; (tcf 'mmt-count (Word "nice"))   ; 7.7624e-9
;
; (tcf 'total-mmt-count)           ;
