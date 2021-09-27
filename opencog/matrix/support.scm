;
; support.scm
;
; Define object-oriented class API's for computing the supporting set
; and the lp-norms of the rows and columns (vectors) in a matrix.
;
; Copyright (c) 2017, 2018 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See object-api.scm for the overview.  Or the README.md file.
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define*-public (add-support-api LLOBJ
	 #:optional (ID (LLOBJ 'id)))
"
  add-support-api LLOBJ ID - Extend LLOBJ with methods to retrieve
  support, count and length subtotals on rows and columns. It is assumed
  that these have been previously computed, as described below.
  See the documentation on `add-support-compute` for precise definitions
  of \"support\", \"amplitude\", \"count\" and \"length\"; in brief,
  these are just the l_0, l_0.5, l_1 and l_2 (Banach lp-space) norms of
  the rows and columns.

  This object provides per-row/per-column values for support, count and
  length.  The `add-report-api` has methods with similar, or the same
  names, but it provides matrix-wide averages (i.e. averaged over all
  rows and columns).

  This object fetches precomputed values, fetched from the \"margins\"
  of the matrix (i.e. attached to the matrix wild-cards.) These marginal
  values must have been previously computed and attached to the wildcards.
  This can be done by saying
     `((add-support-compute LLOBJ) 'cache-all)`
  The `add-support-api` and `add-support-compute` API's are designed
  to work together and complement one-another.

  Optional argument ID is #f to use the default value key; otherwise
  a filtered key is used. That is, the marginals are fetched from a
  default location; however, that location can be changed by specifying
  it with the optional ID argument.
"
	; ----------------------------------------------------

	; Key under which the matrix dimensions are stored.
	; Note that the report object already uses *-Dimension Key-*
	; to hold exactly the same values. We duplicate that data here,
	; because we want to avoid the overhead of the graph centrality
	; computations that the report object does.
	(define is-filtered? (and ID (LLOBJ 'filters?)))
	(define dim-key (PredicateNode
		(if is-filtered?
			(string-append "*-Supp Dimension Key " ID)
			"*-Supp Dimension Key-*")))

	(define wild-atom (LLOBJ 'wild-wild))

	(define (set-size LEFT RIGHT NPAIRS)
		(cog-set-value! wild-atom dim-key (FloatValue LEFT RIGHT NPAIRS)))

	; Use round to force return of integer.
	(define (get-left-dim)
		(inexact->exact (round
			(cog-value-ref (cog-value wild-atom dim-key) 0))))

	(define (get-right-dim)
		(inexact->exact (round
			(cog-value-ref (cog-value wild-atom dim-key) 1))))

	(define (get-num-pairs)
		(inexact->exact (round
			(cog-value-ref (cog-value wild-atom dim-key) 2))))

	; ----------------------------------------------------
	; Key under which the matrix l_p norms are stored.
	(define key-name
		(if is-filtered?
			(string-append "*-Norm Key " ID)
			"*-Norm Key-*"))

	(define norm-key (PredicateNode key-name))

	(define (set-norms ATOM L0 L1 L2 LQ)
		(cog-set-value! ATOM norm-key (FloatValue L0 L1 L2 LQ)))

	; -----------------
	; Set the grand-total count. Use the CountTruthValue.
	; Backwards-compatibility method. Remove this someday.
	(define (set-wild-wild-count CNT)
		(cog-set-tv! (LLOBJ 'wild-wild) (CountTruthValue 0 0 CNT)))

	; -----------------
	(define left-total-key-name
		(if is-filtered?
			(string-append "*-Left Total Key " ID)
			"*-Left Total Key-*"))

	(define left-total-key (PredicateNode left-total-key-name))

	(define (set-left-totals L0 L1)
		(set-wild-wild-count L1)
		(cog-set-value! (LLOBJ 'wild-wild) left-total-key (FloatValue L0 L1)))

	(define right-total-key-name
		(if (and ID (LLOBJ 'filters?))
			(string-append "*-Right Total Key " ID)
			"*-Right Total Key-*"))

	(define right-total-key (PredicateNode right-total-key-name))

	(define (set-right-totals L0 L1)
		(set-wild-wild-count L1)
		(cog-set-value! (LLOBJ 'wild-wild) right-total-key (FloatValue L0 L1)))

	; -----------------
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

	(define (get-amplitude ATOM)
		(catch 'wrong-type-arg
			(lambda () (cog-value-ref (cog-value ATOM norm-key) 3))
			(lambda (key . args) 0)))

	;--------
	(define (get-left-support ITEM)
		(get-support (LLOBJ 'left-wildcard ITEM)))

	(define (get-left-count ITEM)
		(get-count (LLOBJ 'left-wildcard ITEM)))

	(define (get-left-length ITEM)
		(get-length (LLOBJ 'left-wildcard ITEM)))

	(define (get-left-amplitude ITEM)
		(get-amplitude (LLOBJ 'left-wildcard ITEM)))

	(define (set-left-norms ITEM L0 L1 L2 LQ)
		(set-norms (LLOBJ 'left-wildcard ITEM) L0 L1 L2 LQ))

	;--------
	(define (get-right-support ITEM)
		(get-support (LLOBJ 'right-wildcard ITEM)))

	(define (get-right-count ITEM)
		(get-count (LLOBJ 'right-wildcard ITEM)))

	(define (get-right-length ITEM)
		(get-length (LLOBJ 'right-wildcard ITEM)))

	(define (get-right-amplitude ITEM)
		(get-amplitude (LLOBJ 'right-wildcard ITEM)))

	(define (set-right-norms ITEM L0 L1 L2 LQ)
		(set-norms (LLOBJ 'right-wildcard ITEM) L0 L1 L2 LQ))

	;--------
	(define (error-no-data)
		(throw 'no-data 'add-support-api
			(string-append
"There isn't any cached data on " ID "\n"
"Run `((add-support-compute LLOBJ) 'cache-all)` to compute that data.\n")
				))

	(define (get-total-support-left)
		(catch 'wrong-type-arg
			(lambda() (cog-value-ref (cog-value (LLOBJ 'wild-wild) left-total-key) 0))
			(lambda (key . args) (error-no-data))))

	(define (get-total-count-left)
		(catch 'wrong-type-arg
			(lambda() (cog-value-ref (cog-value (LLOBJ 'wild-wild) left-total-key) 1))
			(lambda (key . args) (error-no-data))))

	(define (get-total-support-right)
		(catch 'wrong-type-arg
			(lambda() (cog-value-ref (cog-value (LLOBJ 'wild-wild) right-total-key) 0))
			(lambda (key . args) (error-no-data))))

	(define (get-total-count-right)
		(catch 'wrong-type-arg
			(lambda() (cog-value-ref (cog-value (LLOBJ 'wild-wild) right-total-key) 1))
			(lambda (key . args) (error-no-data))))

	;--------
	; Backwards-compatibility method. Remove this someday.
	; Note that various old datasets store wild-card counts here,
	; and so this method is explicitly needed to access old data.
	; The old data does not have the support-totals, above.
	; Return the grand-total count. Use the CountTruthValue.
	(define (get-wild-wild-count)
		(cog-tv-count (cog-tv (LLOBJ 'wild-wild))))

	;-------------------------------------------
	; Force data to be recomputed, but clobbering any
	; existing data.
	(define (clobber)
		(for-each (lambda (ATOM)
			(cog-set-value! (LLOBJ 'right-wildcard ATOM) norm-key #f))
			((add-pair-stars  LLOBJ) 'left-basis))

		(for-each (lambda (ATOM)
			(cog-set-value! (LLOBJ 'left-wildcard ATOM) norm-key #f))
			((add-pair-stars  LLOBJ) 'right-basis))

		(cog-set-value! (LLOBJ 'wild-wild) left-total-key #f)
		(cog-set-value! (LLOBJ 'wild-wild) right-total-key #f)
		(LLOBJ 'clobber)
	)

	;-------------------------------------------

	(define (help)
		(format #t
			(string-append
"This is the `add-support-api` object applied to the \"~A\"\n"
"object.  It provides methods to access the support, count and length\n"
"subtotals on rows and columns. These must have been previously computed\n"
"using the `add-support-compute` object. See the documentation for\n"
"`add-support-compute` for precise definitions of \"support\", \"count\"\n"
"and \"length\".\n"
"\n"
"For more information, say `,d add-support-api` at the guile prompt,\n"
"or just use the 'describe method on this object. You can also get at\n"
"the base object with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
			(LLOBJ 'id)))

	(define (describe)
		(display (procedure-property add-support-api 'documentation)))

	;--------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((left-dim)           (get-left-dim))
			((right-dim)          (get-right-dim))
			((num-pairs)          (get-num-pairs))

			((left-support)       (apply get-left-support args))
			((right-support)      (apply get-right-support args))
			((left-count)         (apply get-left-count args))
			((right-count)        (apply get-right-count args))
			((left-length)        (apply get-left-length args))
			((right-length)       (apply get-right-length args))
			((left-amplitude)     (apply get-left-amplitude args))
			((right-amplitude)    (apply get-right-amplitude args))

			((total-support-left) (get-total-support-left))
			((total-support-right)(get-total-support-right))
			((total-count-left)   (get-total-count-left))
			((total-count-right)  (get-total-count-right))

			; The 'wild-wild-count method provides backwards-compat
			; with the old `add-pair-count-api` object. Remove whenever.
			((wild-wild-count)    (get-wild-wild-count))

			((set-size)           (apply set-size args))
			((set-left-norms)     (apply set-left-norms args))
			((set-right-norms)    (apply set-right-norms args))
			((set-left-totals)    (apply set-left-totals args))
			((set-right-totals)   (apply set-right-totals args))

			((clobber)            (clobber))
			((help)               (help))
			((describe)           (describe))
			((obj)                "add-support-api")
			((base)               LLOBJ)

			(else                 (apply LLOBJ (cons message args)))))
)

; ---------------------------------------------------------------------

(define*-public (add-support-compute LLOBJ
	 #:optional (GET-CNT 'get-count))
"
  add-support-compute LLOBJ - Extend LLOBJ with methods to
  compute wild-card sums, including the support (lp-norm for p=0),
  the amplitude (lp-norm for p=0.5), the count (lp-norm for p=1),
  the Euclidean length (lp-norm for p=2) and the general lp-norm.
  By default, these are computed from the counts on the matrix;
  optionally, a different source of numbers can be used.  This object
  does not make use of any pre-computed (marginal or \"cached\")
  values; instead, all computations are done on the raw matrix data.
  The computed norms are not placed back into the AtomSpace after
  being computed (unless the 'cache-all method is invoked, in which
  case a bulk computation is done.) Cached values can be access with
  the `add-support-api` object.

  This object provides per-row/per-column values for these quantities.
  The `make-central-compute` object has methods with similar or the
  same names; they provide the matrix-wide averages.

  The 'cache-all method computes norms for the ENTIRE matrix, and
  places them in the margins, i.e. as values on the wild-cards of the
  matrix.  This can take a lot of CPU-time. After the 'cache-all
  method has been invoked, the `(add-support-api)` object can be
  used to access these values.

  In order for 'cache-all to work, the full matrix must available
  in RAM.  It can be fetched by calling `(LLOBJ 'fetch-pairs)`.
  After computing the marginals, it is wise to store them back to
  disk. This can be done with `((make-store LLOBJ) 'store-wildcards)`

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  Let D(x,y) == 1 if N(x,y) > 0; otherwise D(x,y) == 0.

  The 'left-support-set method return all pairs (x,y), for fixed y, for
  which N(x,y) > 0. The right-support-set is the same, for fixed x.

  The support is the size of the support-set.  AKA the l_0 norm.
  The 'left-support is the number of non-zero entries in a column.
  That is, the left-support is D(*,y) = sum_x D(x,y)

  The 'left-count is the wild-card N(*,y) = sum_x N(x,y) for fixed y.
  That is, for a given column y, this sums all counts in that column.

  The 'left-length is sqrt(sum_x N^2(x,y)) for fixed y.

  The 'left-amplitude is (sum_x N^0.5(x,y))^2 for fixed y.

  The 'left-lp-norm is |sum_x N^p(x,y)|^1/p for fixed y.

  The 'total-support is sum_x sum_y D(x,y)
  That is, the total number of non-zero entries in the matrix.

  The 'total-count-left is N(*,*) = sum_x N(x,*)
  That is, the total of all count entries in the matrix, with the
  left-sum being done last. It uses the cached, previously-computed
  right-marginal sums N(x,*) to perform the computation, and so this
  computation will fail, if the marginals have not been stored.

  The 'total-count-right is N(*,*) = sum_y N(*,y)
  Same as above, but does the right-sum last. Should yeild the same
  answer, as above, except for rounding errors. Using this method can
  be more convenient, if the right-marginal sums are not available
  (and v.v. if the other marginals are not available.)

  The 'set-left-marginals ITEM requires an argument ITEM from the
  right basis. It computes the marginals for that ITEM and caches
  them.  This is useful when some algorithm has modified the matrix,
  and the marginals for a specific column need to be recomputed.

  The 'set-right-marginals ITEM requires an argument ITEM from the
  left basis. It computes the marginals for that ITEM and caches them.
  This is useful when some algorithm has modified the matrix, and the
  marginals for a specific row need to be recomputed.

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
	(let* ((star-obj (add-pair-stars LLOBJ))
			(api-obj (add-support-api star-obj))
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
		; Return the sum of probability amplitudes
		(define (sum-amplitude LIST)
			(define tot
				(fold
					(lambda (lopr sum)
						(define cnt (get-cnt lopr))
						(+ sum (sqrt cnt)))
					0
					LIST))
			(* tot tot))

		; Returns the sum of probability amplitudes aka the l_0.5 norm
		; (l_p norm for p=0.5)
		(define (sum-left-amplitude ITEM)
			(sum-amplitude (star-obj 'left-stars ITEM)))

		(define (sum-right-amplitude ITEM)
			(sum-amplitude (star-obj 'right-stars ITEM)))

		; -------------
		; Return the lp-norm (Banach-space norm) of the counts
		; on LIST.  Viz (sum_k N^p(k))^1/p for counted-pairs k
		; in the list
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

		; Compute the total number of times that all pairs have been
		; observed. In formulas, return
		;     N(*,*) = sum_x N(x,*) = sum_x sum_y N(x,y)
		;
		; This method assumes that the right-partial wild-card counts
		; have been previously computed and cached.  That is, it assumes
		; that the 'right-wild-count returns a valid value. This value
		; should be the same as what 'compute-right-count would return.
		(define (compute-total-count-from-right)
			(fold
				;;; Use the cached value, equiavalent to this:
				;;; (lambda (item sum) (+ sum (sum-right-count item)))
				(lambda (item sum) (+ sum (api-obj 'right-count item)))
				0
				(star-obj 'left-basis)))

		; Compute the total number of times that all pairs have been
		; observed. That is, return N(*,*) = sum_y N(*,y). Note that
		; this should give exactly the same result as the above; however,
		; the order in which the sums are performed is distinct, and
		; thus large differences indicate a bug; small differences are
		; due to rounding errors.
		(define (compute-total-count-from-left)
			(fold
				;;; (lambda (item sum) (+ sum (sum-left-count item)))
				(lambda (item sum) (+ sum (api-obj 'left-count item)))
				0
				(star-obj 'right-basis)))

		; Same as above, but for the support
		(define (compute-total-support-from-right)
			(fold
				; (lambda (item sum) (+ sum (get-right-support-size item)))
				(lambda (item sum) (+ sum (api-obj 'right-support item)))
				0
				(star-obj 'left-basis)))

		(define (compute-total-support-from-left)
			(fold
				; (lambda (item sum) (+ sum (get-left-support-size item)))
				(lambda (item sum) (+ sum (api-obj 'left-support item)))
				0
				(star-obj 'right-basis)))

		; -------------
		; Compute all l_0, l_0.5, l_1 and l_2 norms, attach them
		; to the wildcards, where the support-api can find them.

		; Perform three sums at once. The final sqrt taken later.
		(define (sum-norms LIST)
			(fold
				(lambda (lopr sum)
					(define cnt (get-cnt lopr))
					(if (< 0 cnt) (list
							(+ (first sum) 1)
							(+ (second sum) cnt)
							(+ (third sum) (* cnt cnt))
							(+ (fourth sum) (sqrt cnt)))
						sum))
				(list 0 0 0 0)
				LIST))

		(define (sum-left-norms ITEM)
			(sum-norms (star-obj 'left-stars ITEM)))

		(define (sum-right-norms ITEM)
			(sum-norms (star-obj 'right-stars ITEM)))

		(define (set-left-marginals ITEM)
			(define sums (sum-left-norms ITEM))
			(define l0 (first sums))
			(define l1 (second sums))
			(define l2 (sqrt (third sums)))
			(define lq (* (fourth sums) (fourth sums)))
			(api-obj 'set-left-norms ITEM l0 l1 l2 lq))

		(define (set-right-marginals ITEM)
			(define sums (sum-right-norms ITEM))
			(define l0 (first sums))
			(define l1 (second sums))
			(define l2 (sqrt (third sums)))
			(define lq (* (fourth sums) (fourth sums)))
			(api-obj 'set-right-norms ITEM l0 l1 l2 lq))

		; ----------------------------------------------------

		(define (set-dimensions NPAIRS)
			(api-obj 'set-size
				(star-obj 'left-basis-size)
				(star-obj 'right-basis-size)
				NPAIRS))

		(define (all-left-marginals)
			(define elapsed-secs (make-elapsed-secs))

			; Loop over each item in the right-basis
			(maybe-par-for-each set-left-marginals (star-obj 'right-basis))
			(format #t "Finished left norm marginals in ~A secs\n"
				(elapsed-secs))

			; Totals can only be computed, after above has been cached.
			(api-obj 'set-left-totals
				(compute-total-support-from-left)
				(compute-total-count-from-left))

			; total-support-left should equal total-support-right
			(set-dimensions (api-obj 'total-support-left))

			(format #t "Finished left totals in ~A secs\n"
				(elapsed-secs))
		)

		(define (all-right-marginals)
			(define elapsed-secs (make-elapsed-secs))

			; Loop over each item in the left-basis
			(maybe-par-for-each set-right-marginals (star-obj 'left-basis))
			(format #t "Finished right norm marginals in ~A secs\n"
				(elapsed-secs))

			; Totals can only be computed, after above has been cached.
			(api-obj 'set-right-totals
				(compute-total-support-from-right)
				(compute-total-count-from-right))

			; total-support-left should equal total-support-right
			(set-dimensions (api-obj 'total-support-right))

			(format #t "Finished right totals in ~A secs\n"
				(elapsed-secs))
		)

		; Do both at once
		(define (cache-all)
			(all-left-marginals)
			(all-right-marginals))

		; Force data to be recomputed, by clobbering any
		; existing data.
		(define (clobber)
			(api-obj 'clobber)
			(star-obj 'clobber)
		)

		;-------------------------------------------

		(define (help)
			(format #t
				(string-append
"This is the `add-support-compute` object applied to the \"~A\"\n"
"object.  It provides methods to compute the support, size and length\n"
"subtotals on rows and columns. It is recommended that this object be\n"
"used only to precompute and cache these values, which can then be more\n"
"quickly accessed with the `add-support-api` object.\n"
"\n"
"For more information, say `,d add-support-compute` at the guile prompt,\n"
"or just use the 'describe method on this object. You can also get at\n"
"the base object with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
				(LLOBJ 'id)))

		(define (describe)
			(display (procedure-property add-support-compute 'documentation)))

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
				((left-amplitude)     (apply sum-left-amplitude args))
				((right-amplitude)    (apply sum-right-amplitude args))
				((left-lp-norm)       (apply sum-left-lp-norm args))
				((right-lp-norm)      (apply sum-right-lp-norm args))

				((total-support-left)  (compute-total-support-from-left))
				((total-support-right) (compute-total-support-from-right))
				((total-count-left)    (compute-total-count-from-left))
				((total-count-right)   (compute-total-count-from-right))

				((set-left-marginals)  (apply set-left-marginals args))
				((set-right-marginals) (apply set-right-marginals args))

				((all-left-marginals)  (all-left-marginals))
				((all-right-marginals) (all-right-marginals))
				((cache-all)           (cache-all))
				((clobber)             (clobber))

				((help)                (help))
				((describe)            (describe))
				((obj)                 "add-support-compute")
				((base)                LLOBJ)

				(else                  (apply LLOBJ (cons message args))))
			)))

; ---------------------------------------------------------------------
