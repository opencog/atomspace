;
; filter.scm
;
; Define API's for filtering the matrixes, e.g. by removing entries
; with low counts.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Some types of analysis, e.g. the thresholding-PCA code, will provide
; better results if some of the noise in the data is removed.  In this
; case, noise is considered to be any rows or columns that have subtotal
; column counts below a certain value: anything that was observed very
; infrequently.
;
; This overloads the "star" API to provide the filtered dataset.
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------

(define-public (add-generic-filter LLOBJ
	LEFT-BASIS-PRED RIGHT-BASIS-PRED
	LEFT-STAR-PRED RIGHT-STAR-PRED
	PAIR-PRED)
"
  add-generic-filter LLOBJ - Modify LLOBJ so that only the columns and
  rows that satisfy the predicates are retained.

  The LEFT-BASIS-PRED and RIGHT-BASIS-PRED should be functions that
  accept atoms in the left and right basis, and return #t if they
  should be kept.

  The LEFT-STAR-PRED and RIGHT-STAR-PRED should be functions that
  accept left and right wild-card pairs, and return #t if they should
  be kept.

  The PAIR-PRED should be a function to that accepts individual matrix
  entries. It is applied whenever the 'item-pair or 'pair-count methods
  are invoked.  Like the others, it should return #t to keep the pair.
"
	(let ((stars-obj (add-pair-stars LLOBJ))
			(l-basis '())
			(r-basis '())
			(l-size 0)
			(r-size 0)
		)

		; ---------------
		; Filter out rows and columns that pass the left and right
		; predicates.
		(define (do-left-basis)
			(filter LEFT-BASIS-PRED (stars-obj 'left-basis)))

		(define (do-right-basis)
			(filter RIGHT-BASIS-PRED (stars-obj 'right-basis)))

		; ---------------
		; Use the cached value, if its there.
		(define (get-left-basis)
			(if (null? l-basis) (set! l-basis (do-left-basis)))
			l-basis)

		(define (get-right-basis)
			(if (null? r-basis) (set! r-basis (do-right-basis)))
			r-basis)

		(define (get-left-size)
			(if (eq? 0 l-size) (set! l-size (length (get-left-basis))))
			l-size)

		(define (get-right-size)
			(if (eq? 0 r-size) (set! r-size (length (get-right-basis))))
			r-size)

		; ---------------
		; Return only those stars that pass the cutoff.
		;
		(define (do-left-stars ITEM)
			(filter
				(lambda (PAIR)
					(and (LEFT-STAR-PRED PAIR) (PAIR-PRED PAIR)))
				(stars-obj 'left-stars ITEM)))

		(define (do-right-stars ITEM)
			(filter
				(lambda (PAIR)
					(and (RIGHT-STAR-PRED PAIR) (PAIR-PRED PAIR)))
				(stars-obj 'right-stars ITEM)))

		; Cache the results above, so that we don't recompute over and over.
		(define cache-left-stars (make-afunc-cache do-left-stars))
		(define cache-right-stars (make-afunc-cache do-right-stars))

		; ---------------
		; Apply the pair-cut to each pair.
		(define (get-item-pair PAIR)
			(if (PAIR-PRED PAIR) (LLOBJ 'item-pair PAIR) '()))

		(define (get-pair-count PAIR)
			(if (PAIR-PRED PAIR) (LLOBJ 'pair-count PAIR) 0))

		; ---------------
		; Return a pointer to each method that this class overloads.
		(define (provides meth)
			(case meth
				((left-stars)       cache-left-stars)
				((right-stars)      cache-right-stars)
				((left-basis)       get-left-basis)
				((right-basis)      get-right-basis)
				((left-basis-size)  get-left-size)
				((right-basis-size) get-right-size)
				(else               (LLOBJ 'provides meth))))

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-stars)       (apply cache-left-stars args))
				((right-stars)      (apply cache-right-stars args))
				((left-basis)       (get-left-basis))
				((right-basis)      (get-right-basis))
				((left-basis-size)  (get-left-size))
				((right-basis-size) (get-right-size))
				((item-pair)        (apply get-item-pair args))
				((pair-count)       (apply get-pair-count args))
				((provides)         (apply provides args))
				((filters?)         (lambda () #t))
				; Pass through some selected methods
				((left-type)        (apply LLOBJ (cons message args)))
				((right-type)       (apply LLOBJ (cons message args)))
				((pair-type)        (apply LLOBJ (cons message args)))
				; Block anything that might have to be filtered.
				; For example: 'pair-freq which we don't, can't filter.
				; Or any of the variious subtotals.
				(else               (throw 'bad-use 'add-generic-filter
					(format #f "Sorry, method ~A not available on filter!" message))))
		)))

; ---------------------------------------------------------------------

(define-public (add-subtotal-filter LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT)
"
  add-subtotal-filter LLOBJ - Modify LLOBJ so that any columns and
  rows with counts less than LEFT-CUT and RIGHT-CUT are removed, and that
  individual entries with counts less than PAIR-CUT are removed. This
  provides an API compatible with the star-object API; i.e. it provides
  the same row and column addressability that star-object does, but
  just returns fewer rows and columns.

  Thhe filtering is done 'on demand', on a row-by-row, column-by-column
  basis.  Currenly, computations for the left and right stars are not
  cached, and are recomputed for each request.  Currently, this seems
  like a reasonable thing to do.

  Note that by removing rows and columns, the frequencies will no longer
  sum to 1.0. Likewise, row and column subtotals, entropies and mutual
  information will no long sum or behave as in the whole dataset.  If
  accurate values for these are needed, then they would need to be
  recomputed for the reduced matrix.

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  Let N(*,y) be the column subtotals, AKA the left-subtotals.
  Let N(x,*) be the row subtotals, AKA the right subtotals.

  This object removes all columns where  N(*,y) <= RIGHT-CUT and where
  N(x,*) <= LEFT-CUT.  Pairs are not reported in the 'left-stars and
  'right-stars methods when N(x,y) <= PAIR-CUT.

  The net effect of the cuts is that when LEFT-CUT is increased, the
  left-dimension of the dataset drops; likewise on the right.
"
	(let* ((stars-obj (add-pair-stars LLOBJ))
			(cnt-obj (add-pair-count-api stars-obj))
		)

		; ---------------
		; Filter out rows and columns that are below-count.
		;
		; Yes, we want LEFT-CUT < right-wild-count this looks weird,
		; but is correct: as LEFT-CUT gets larger, the size of the
		; left-basis shrinks.
		(define (left-basis-pred ITEM)
			(< LEFT-CUT (cnt-obj 'right-wild-count ITEM)))

		(define (right-basis-pred ITEM)
			(< RIGHT-CUT (cnt-obj 'left-wild-count ITEM)))

		; ---------------
		; Return only those stars that pass the cutoff.
		;
		; See comments above: LEFT-CUT < right-wild-count is correct.
		(define (left-stars-pred PAIR)
			(< LEFT-CUT (cnt-obj 'right-wild-count (gar PAIR))))

		(define (right-stars-pred PAIR)
			(< RIGHT-CUT (cnt-obj 'left-wild-count (gdr PAIR))))

		(define (pair-pred PAIR)
			(< PAIR-CUT (LLOBJ 'pair-count PAIR)))

		; ---------------
		(add-generic-filter LLOBJ
			left-basis-pred right-basis-pred
			left-stars-pred right-stars-pred
			pair-pred)
	)
)

; ---------------------------------------------------------------------

(define-public (add-knockout-filter LLOBJ LEFT-KNOCKOUT RIGHT-KNOCKOUT)
"
  add-knockout-filter LLOBJ - Modify LLOBJ so that the explicitly
  indicated rows and columns are removed.
"
	; ---------------
	; Filter out rows and columns in the knockout lists.
	;
	(define (left-basis-pred ITEM)
		(not (any
			(lambda (knockout) (equal? knockout ITEM))
			LEFT-KNOCKOUT)))

	(define (right-basis-pred ITEM)
		(not (any
			(lambda (knockout) (equal? knockout ITEM))
			RIGHT-KNOCKOUT)))

	; ---------------
	; Return only those stars that pass the cutoff.
	(define (left-stars-pred PAIR)
		(left-basis-pred (gar PAIR)))

	(define (right-stars-pred PAIR)
		(right-basis-pred (gdr PAIR)))

	(define (pair-pred PAIR)
		(and (left-stars-pred PAIR) (right-stars-pred PAIR)))

	; ---------------
	(add-generic-filter LLOBJ
		left-basis-pred right-basis-pred
		left-stars-pred right-stars-pred
		pair-pred)
)

; ---------------------------------------------------------------------
