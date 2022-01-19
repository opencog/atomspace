;
; entropy.scm
;
; Assorted objects for computing (and caching) entropies and marginal
; entropies of matricies.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; The object here assumes that a pair-counting batch job has completed.
; That is, it assumes that there are cached values available for the
; individual pair frequencies, the wild-card frequencies, and the
; individual pair MI values.
;
; It uses these cached stats for frequencies and MI to compute left
; and right subtotals (row and column subtotals), which are then cached.
; The cached values become available via the standard frequency API.
;
; XXX FIXME: This should be re-written to avoid the frequency API, and
; make use only of the counts during computation. There are two reasons
; for this:
; 1) It takes time to compute the frequencies, and it takes storage to
;    store them. This is a waste for large datasets; and poses problems
;    when the datasets are extremely large.
; 2) The count api can be filtered in a straight-forward way, with the
;    filtering API. But these filtered counts are not used below, and
;    there's no way to use that, without recomputing the frequencies.
;    So .. see point 1) above: frequencies are screwy when filtering.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))

; ---------------------------------------------------------------------

; For now, we are not making this public, I see no point to this.
; The super whiz-bang batch job will use this, but I just don't see
; why  the general public would want direct access to this.  So, for
; now, we hide it.
;
; Also -- FYI, we could provide methods to return just the computed
; values, without caching them, but I don't see the point of this.
;
(define-public (add-entropy-compute LLOBJ)
"
  add-entropy-compute LLOBJ - methods for computing and caching
  the subtotalled MI and entropy of rows and columns.

  Extend the LLOBJ with additional methods to compute the one-sided
  entropies and mutual information of pairs.

  The object must have valid pair-frequency values on it, accessible
  via the standard frequency-object API. These must have been
  pre-computed, before this object can be used.

  The methods on this class are:
  'cache-left-entropy   -- compute and cache the column entropies
  'cache-right-entropy  -- compute and cache the row entropies
  'cache-left-mi        -- compute and cache the column mi
  'cache-right-mi       -- compute and cache the row mi

  The cached values are accessible via the standard frequency API.
"
	; Need the 'left-stars method, provided by add-pair-stars
	; Need the 'left-wild-freq method, provided by add-pair-freq-api
	;     We don't want it to throw, in case some pair has zero counts.
	(let* ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ))
			(frqobj (add-pair-freq-api star-obj #:nothrow #t)))

		; Compute the left-wild entropy summation:
		;    h_left(y) = -sum_x P(x,y) log_2 P(x,y)
		;
		; Note that
		;    h_total = sum_y h_left(y)
		(define (compute-left-entropy RIGHT-ITEM)
			(fold
				(lambda (PAIR sum) (+ sum (frqobj 'pair-entropy PAIR)))
				0
				(star-obj 'left-stars RIGHT-ITEM)))

		; Compute the right-wild entropy summation:
		;    h_right(x) = -sum_y P(x,y) log_2 P(x,y)
		;
		; Note that
		;    h_total = sum_x h_right(x)
		(define (compute-right-entropy LEFT-ITEM)
			(fold
				(lambda (PAIR sum) (+ sum (frqobj 'pair-entropy PAIR)))
				0
				(star-obj 'right-stars LEFT-ITEM)))

		; Compute the left-fractional entropy summation:
		;    H_left(y) = h_left(y) / P(*,y)
		; Note that
		;    h_total = sum_y P(*,y) H_left(y)
		(define (cache-left-entropy RIGHT-ITEM)
			(define ent (compute-left-entropy RIGHT-ITEM))
			(define fent (/ ent (frqobj 'left-wild-freq RIGHT-ITEM)))
			(frqobj 'set-left-wild-entropy RIGHT-ITEM ent fent))

		; As above, but flipped.
		(define (cache-right-entropy LEFT-ITEM)
			(define ent (compute-right-entropy LEFT-ITEM))
			(define fent (/ ent (frqobj 'right-wild-freq LEFT-ITEM)))
			(frqobj 'set-right-wild-entropy LEFT-ITEM ent fent))

		; ---------------
		; Compute the left MI summation:
		;    mi_left(y) = sum_x P(x,y) log_2 MI(x,y)
		;
		; where MI(x,y) = -log_2 P(x,y) / P(x,*) P(*,y)
		;
		; Note that
		;    MI_total = sum_y mi_left(y)
		(define (compute-left-mi RIGHT-ITEM)
			(fold
				(lambda (PAIR sum)
					; MI might be inf. if count is zero...
					(define pmi (frqobj 'pair-mi PAIR))
					(if (finite? pmi) (+ sum pmi) sum))
				0
				(star-obj 'left-stars RIGHT-ITEM)))

		; As above, but flipped.
		(define (compute-right-mi LEFT-ITEM)
			(fold
				(lambda (PAIR sum)
					(define pmi (frqobj 'pair-mi PAIR))
					(if (finite? pmi) (+ sum pmi) sum))
				0
				(star-obj 'right-stars LEFT-ITEM)))

		; Compute the left-fractional MI summation:
		;    MI_left(y) = mi_left(y) / P(*,y)
		; Note that
		;    MI_total = sum_y P(*,y) MI_left(y)
		(define (cache-left-mi RIGHT-ITEM)
			(define mi (compute-left-mi RIGHT-ITEM))
			(define fmi (/ mi (frqobj 'left-wild-freq RIGHT-ITEM)))
			(frqobj 'set-left-wild-mi RIGHT-ITEM mi fmi))

		; As above, but flipped.
		(define (cache-right-mi LEFT-ITEM)
			(define mi (compute-right-mi LEFT-ITEM))
			(define fmi (/ mi (frqobj 'right-wild-freq LEFT-ITEM)))
			(frqobj 'set-right-wild-mi LEFT-ITEM mi fmi))

		; ---------------

		; Loop over all columns.
		(define (cache-all-left-entropy)
			(define elapsed-secs (make-elapsed-secs))

			(maybe-par-for-each cache-left-entropy (star-obj 'right-basis))
			(format #t "Finished left entropy subtotals in ~A secs\n"
				(elapsed-secs))
		)

		; Loop over all rows.
		(define (cache-all-right-entropy)
			(define elapsed-secs (make-elapsed-secs))

			(maybe-par-for-each cache-right-entropy (star-obj 'left-basis))
			(format #t "Finished right entropy subtotals in ~A secs\n"
				(elapsed-secs))
		)

		; Loop over all columns.
		(define (cache-all-left-mi)
			(define elapsed-secs (make-elapsed-secs))

			(maybe-par-for-each cache-left-mi (star-obj 'right-basis))
			(format #t "Finished left MI subtotals in ~A secs\n"
				(elapsed-secs))
		)

		; Loop over all rows.
		(define (cache-all-right-mi)
			(define elapsed-secs (make-elapsed-secs))

			(maybe-par-for-each cache-right-mi (star-obj 'left-basis))
			(format #t "Finished right MI subtotals in ~A secs\n"
				(elapsed-secs))
		)

		; Do all four loops.
		(define (cache-all)
			(cache-all-left-entropy)
			(cache-all-right-entropy)
			(cache-all-left-mi)
			(cache-all-right-mi)
		)

		; Methods on this class.
		(lambda (message . args)
			(case message
				((cache-left-entropy)   (apply cache-left-entropy args))
				((cache-right-entropy)  (apply cache-right-entropy args))
				((cache-left-mi)        (apply cache-left-mi args))
				((cache-right-mi)       (apply cache-right-mi args))

				((cache-all-left-entropy)  (cache-all-left-entropy))
				((cache-all-right-entropy) (cache-all-right-entropy))
				((cache-all-left-mi)       (cache-all-left-mi))
				((cache-all-right-mi)      (cache-all-right-mi))

				((cache-all-subtotals)  (cache-all))
				(else (apply llobj      (cons message args))))
		))
)

; ---------------------------------------------------------------------

(define (add-total-entropy-compute LLOBJ)
"
  add-total-entropy-compute LLOBJ - methods to compute and cache the
  partial and total entropies and the total MI.

  Extend the LLOBJ with additional methods to compute the partial and
  total entropies and MI for the correlation matrix.

  The object must have valid partial sums for the entropy and MI on it,
  viz, the ones computed by add-entropy-compute, above. These are
  accessed via the standard frequency-object API. These must have been
  pre-computed, before this object can be used.

  These methods loop over all rows and columns to compute the total sums.
"
	; Need the 'left-basis method, provided by add-pair-stars
	; Need the 'pair-logli method, provided by add-pair-freq-api
	(let* ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ))
			(frqobj (add-pair-freq-api star-obj))
			(rptobj (add-report-api star-obj))
		)

		(define (left-sum FN)
			(fold
				(lambda (right-item sum) (+ sum (FN right-item)))
				0 (star-obj 'right-basis)))

		(define (right-sum FN)
			(fold
				(lambda (left-item sum) (+ sum (FN left-item)))
				0 (star-obj 'left-basis)))

		; ---------------
		; Compute the total entropy for the set. This loops over all
		; rows and columns, and computes the sum
		;   H_tot = sum_x sum_y p(x,y) log_2 p(x,y)
		;         = sum_x h_left(x)
		;         = sum_y h_right(y)
		; It throws an error if the two are not equal (to within guessed
		; rounding errors.)
		(define (compute-total-entropy)
			(define lsum (left-sum
					(lambda (x) (frqobj 'left-wild-entropy x))))
			(define rsum (right-sum
					(lambda (x) (frqobj 'right-wild-entropy x))))
			(if (< 1.0e-8 (/ (abs (- lsum rsum)) lsum))
				(throw 'bad-summation 'compute-total-entropy
					(format #f
						"Left and right entropy sums fail to be equal: ~A ~A\n"
						lsum rsum)))
			lsum)

		; Compute the left-wildcard partial entropy for the set. This
		; loops over all left-wildcards, and computes the sum
		;   H_left = sum_y p(*,y) log_2 p(*,y)
		; It returns a single numerical value, for the entire set.
		(define (compute-left-entropy)
			(left-sum
				(lambda (x)
					;; In general pairs can have a zero count,
					;; and so a minus-inf logarithm. avoid NaN
					(define lli (frqobj 'left-wild-logli x))
					(if (finite? lli)
						(* (frqobj 'left-wild-freq x) lli)
						0.0))))

		; Compute the right-wildcard partial entropy for the set. This
		; loops over all right-wildcards, and computes the sum
		;   H_right = sum_x p(x,*) log_2 p(x,*)
		; It returns a single numerical value, for the entire set.
		(define (compute-right-entropy)
			(right-sum
				(lambda (x)
					;; For cross-connectors, the observed count of a
					;; word inside a connector might be zero, and so
					;; log probability might be infinite. Avoid that.
					(define lli (frqobj 'right-wild-logli x))
					(if (finite? lli)
						(* (frqobj 'right-wild-freq x) lli)
						0.0))))

		(define (cache-entropy)
			(rptobj 'set-entropy
				(compute-left-entropy)
				(compute-right-entropy)
				(compute-total-entropy)))

		; ---------------
		; Compute the total MI for the set. This loops over all
		; rows and columns, and computes the sum
		;   MI_tot = sum_x sum_y mi(x,y)
		;         = sum_x mi_left(x)
		;         = sum_y mi_right(y)
		; It throws an error if the two are not equal (to within guessed
		; rounding errors.)
		(define (compute-total-mi)
			(define lsum (left-sum
					(lambda (x) (frqobj 'left-wild-mi x))))
			(define rsum (right-sum
					(lambda (x) (frqobj 'right-wild-mi x))))
			(if (< 1.0e-8 (/ (abs (- lsum rsum)) lsum))
				(throw 'bad-summation 'compute-total-mi
					(format #f
						"Left and right MI sums fail to be equal: ~A ~A\n"
						lsum rsum)))
			lsum)

		(define (cache-mi)
			(rptobj 'set-mi (compute-total-mi)))

		; ---------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((total-entropy)         (compute-total-entropy))
				((left-entropy)          (compute-left-entropy))
				((right-entropy)         (compute-right-entropy))
				((cache-entropy)         (cache-entropy))
				((cache-mi)              (cache-mi))
			)))
)

; ---------------------------------------------------------------------
