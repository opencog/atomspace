;
; report-api.scm
;
; Define API providing overview of the correlation matrix.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Each correlation matix has a size - the left and right dimensions, aka
; the number of rows and columns.  These are returned by 'left-dim and
; 'right-dim.
;
; Each matrix is sparse, and has only a small number of non-zero
; entries. These are returned by 'num-pairs.
;
; And so on. See documentation below.

; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog))

; ---------------------------------------------------------------------

(define-public (add-report-api LLOBJ)
"
  add-report-api LLOBJ - Extend LLOBJ with API to provide summary
  statistics for the set of pairs, including the number of rows
  and columns (i.e. the size of the left and right support),
  the total number of pairs (which is the same as the total number
  of non-zero entries in the matrix), the left, right and total
  entropies and the total mutual information.

  Here, the LLOBJ is expected to be an object, with the 'wild-wild
  method on it.  This is the atom on which these summaries will be
  stored.

  The implemented methods return the following values:
  'left-dim         -- The number of rows
  'right-dim        -- The number of columns
  'num-pairs        -- The number of non-zero entries
  'total-count      -- Total number fo observations on all pairs
                       (Identical to the 'wild-wild-count on the
                       count-api object)

  'left-entropy     -- The sum H_left = -sum_x P(x,*) log_2 P(x,*)
  'right-entropy    -- The sum H_right = -sum_y P(*,y) log_2 P(*,y)
  'total-entropy    -- The sum H_tot = sum_x sum_y P(x,y) log_2 P(x,y)
  'total-mi         -- The sum MI = H_tot - H_left - H_right

  'left-support     -- average l_0 norm of rows
  'left-size        -- average l_1 norm of rows
  'left-length      -- average l_2 norm of rows
  'left-hubbiness   -- standard deviation counts in rows

  'right-support    -- average l_0 norm of columns
  'right-size       -- average l_1 norm of columns
  'right-length     -- average l_2 norm of columns
  'right-hubbiness  -- standard deviation counts in columns

  If we imagine each pair as a directed edge, an arrow pointing from
  left to right, then the left-support is the same as the average
  out-degree of the left-vertexes. The right-support is the average
  in-degree of the right-vertexes. Equivalently, the left-support is
  the average number of non-zero entries in each row, and the
  right-support is the average number of non-zero entries in each
  column.

  The left and right sizes are analogous, but are weighted by the
  number of observations on each vertex.

  In formulas:
    Let |(x,*)| = number of non-zero entries for row x.
                = sum_y 1 if [0 < N(x,y) ]
                = l_0 norm of row x.
        size(x) = sum_y N(x,y)
                = N(x,*)
                = l_1 norm of row x.
        len(x) = sqrt[ sum_y N^2(x,y) ]
               = l_2 norm of row x.
               = "length" of row x.
        with analogous values for columns.

        |(*,*)| = total number of non-zero entries.
                = sum_x |(x,*)|
                = sum_y |(*,y)|
        N(*,*)  = total number of observations.

    Then:
        left-support = |(*,*)| / num-rows
        right-support = |(*,*)| / num-columns
        left-size = N(*,*) / num-rows
        right-size = N(*,*) / num-columns
        left-length = [ sum_x p(x) len(x) ]
        right-length = [ sum_y p(y) len(y) ]

    Note that while computing the average length of a row/column,
    this is weighted by the probability of that row/column.

  The hubbiness is defined as sqrt[ (l_2)^2 - (l_1)^2 ].
"
	(let* ((llobj LLOBJ)

			(cntobj (add-pair-count-api LLOBJ))
			(totcnt (cntobj 'wild-wild-count))
			(wild-atom (LLOBJ 'wild-wild))
		)

		; ----------------------------------------------------
		; Key under which the matrix dimensions are stored.
		(define dim-key (PredicateNode "*-Dimension Key-*"))

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
		; Key under which the matrix entropies are stored.
		(define ent-key (PredicateNode "*-Total Entropy Key-*"))

		(define (set-entropy LEFT RIGHT TOT)
			(cog-set-value! wild-atom ent-key (FloatValue LEFT RIGHT TOT)))

		(define (get-left-entropy)
			(cog-value-ref (cog-value wild-atom ent-key) 0))

		(define (get-right-entropy)
			(cog-value-ref (cog-value wild-atom ent-key) 1))

		(define (get-total-entropy)
			(cog-value-ref (cog-value wild-atom ent-key) 2))

		; ----------------------------------------------------
		; Key under which the matrix MI are stored.
		(define mi-key (PredicateNode "*-Total MI Key-*"))

		(define (set-mi TOT)
			(cog-set-value! wild-atom mi-key (FloatValue TOT)))

		(define (get-total-mi)
			(cog-value-ref (cog-value wild-atom mi-key) 0))

		; ----------------------------------------------------
		(define (get-total-count) totcnt)
		(define (get-left-support) (/ (get-num-pairs) (get-left-dim)))
		(define (get-right-support) (/ (get-num-pairs) (get-right-dim)))
		(define (get-left-size) (/ (get-total-count) (get-left-dim)))
		(define (get-right-size) (/ (get-total-count) (get-right-dim)))

		; ----------------------------------------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-dim)            (get-left-dim))
				((right-dim)           (get-right-dim))
				((num-pairs)           (get-num-pairs))
				((set-size)            (apply set-size args))

				((left-entropy)        (get-left-entropy))
				((right-entropy)       (get-right-entropy))
				((total-entropy)       (get-total-entropy))
				((set-entropy)         (apply set-entropy args))

				((total-mi)            (get-total-mi))
				((set-mi)              (apply set-mi args))

				((total-count)         (get-total-count))
				((left-support)        (get-left-support))
				((right-support)       (get-right-support))
				((left-size)           (get-left-size))
				((right-size)          (get-right-size))
			))
	)
)

; ---------------------------------------------------------------------

(define-public (make-central-compute LLOBJ)
"
  add-central-compute LLOBJ - Extend LLOBJ with methods to compute
  misc graph-centrality statistics.
"
	(let* ((llobj LLOBJ)
			(wild-obj (add-pair-stars LLOBJ))
			(len-obj (add-pair-support-compute wild-obj))
			(rpt-obj (add-report-api wild-obj))
			(l-len 0)
			(r-len 0)
		)

		(define (do-get-left-length)
			; The right-length gives the length of one row.
			; The sum is over all the columns, divided by the
			; number of columns.
			(define len
				(fold
					(lambda (sum item)
						(+ sum (len-obj 'right-length item)))
					0
					(star-obj 'left-basis)))
			(/ len (rpt-obj 'left-dim))
		)
		(define (get-left-length)
			(if (eqv? l-len 0) (set! l-len (do-get-left-length)))
			l-len)

		(define (do-get-right-length)
			; The left-length gives the length of one column.
			; The sum is over all the columns, divided by the
			; number of columns.
			(define len
				(fold
					(lambda (sum item)
						(+ sum (len-obj 'left-length item)))
					0
					(star-obj 'right-basis)))
			(/ len (rpt-obj 'right-dim))
		)
		(define (get-right-length)
			(if (eqv? r-len 0) (set! r-len (do-get-right-length)))
			r-len)

		; ---------
		(define (get-left-rms-length)
			(define sz (rpt-obj 'left-size))
			(- (get-left-length) (* sz sz))
		)

		(define (get-right-rms-length)
			(define sz (rpt-obj 'right-size))
			(- (get-right-length) (* sz sz))
		)

		; ----------------------------------------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-length)       (get-left-length))
				((right-length)      (get-right-length))
				((left-rms-length)   (get-left-rms-length))
				((right-rms-length)  (get-right-rms-length))
			))
	)
)

; ---------------------------------------------------------------------

(define-public (print-matrix-summary-report LLOBJ PORT)
"
  print-matrix-summary-report LLOBJ PORT
  Print a summary report about the pair dataset LLOBJ to output PORT
  (typically a string or file port).
"
	(define (log2 x) (/ (log x) (log 2)))

	(define rpt-obj (add-report-api LLOBJ))
	(define cnt-obj (add-pair-count-api LLOBJ))

	(define size (rpt-obj 'num-pairs))
	(define nrows (rpt-obj 'left-dim))
	(define ncols (rpt-obj 'right-dim))
	(define tot (* nrows ncols))
	(define obs (cnt-obj 'wild-wild-count))

	(format PORT "Summary Report for Correlation Matrix ~A\n"
		(LLOBJ 'name))
	(format PORT "Left type: ~A    Right Type: ~A    Pair Type: ~A\n"
		(LLOBJ 'left-type) (LLOBJ 'right-type) (LLOBJ 'pair-type))
	(format PORT "Wildcard: ~A" (LLOBJ 'wild-wild))

	(format PORT "Rows: ~d Columns: ~d\n" nrows ncols)

	(format PORT "Size: ~d non-zero entries of ~d possible\n"
		size tot)
	(format PORT "Fraction non-zero: ~9,4g Sparsity (-log_2): ~6f\n"
		(/ size tot) (log2 (/ tot size)))
	(format PORT "Total observations: ~d  Avg obs per pair: ~6f\n"
		obs (/ obs size))

	(format PORT "Entropy Total: ~6f   Left: ~6f   Right: ~6f\n"
		(rpt-obj 'total-entropy)
		(rpt-obj 'left-entropy)
		(rpt-obj 'right-entropy)
	)
	(format PORT "Total MI: ~6f\n" (rpt-obj 'total-mi))

	(format PORT "\n")
	(format PORT "               Left   Right\n")
	(format PORT "               ----   -----\n")
	(format PORT "Support (l_0)  ~6f    ~6f\n"
		(/ size nrows) (/ size ncols))
	(format PORT "Size    (l_1)  ~6f    ~6f\n"
		(/ obs nrows) (/ obs ncols))
)

; ---------------------------------------------------------------------
