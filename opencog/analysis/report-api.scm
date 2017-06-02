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
        N(x,*)  = sum_y N(x,y)
                = l_1 norm of row x.
        L(x,*) = sqrt[ sum_y N^2(x,y) ]
               = l_2 norm of row x.
               = 'length' of row x.
        with analogous values for columns.

        |(*,*)| = total number of non-zero entries.
                = sum_x |(x,*)|
        N(*,*)  = total number of observations.
                = sum_x N(x,*)

    The probability of observing a row is
        P(x,*) = N(x,*) / N(*,*)

    Then we define these weighted averages:
        left-support = sum_x P(x,*) |(x,*)|
        left-size = sum_x P(x,*) N(x,*)
        left-length = sqrt [ sum_x P(x,*) L(x,*) ]
        left-rms-length = sqrt [ sum_x P(x,*)
               [ L(x,*) - (N(x,*))^2 ] ]

    Note that while computing the average length of a row/column,
    this is weighted by the probability of that row/column.

    The rms-length is the squeare root of what R. Ferrer i Cancho
    calls 'hubbiness' (his hubbiness is the 2nd central moment, if
    I recall correctly).
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
		; Key under which the matrix l_p norms are stored.
		(define l-norm-key (PredicateNode "*-Left Norm Key-*"))
		(define r-norm-key (PredicateNode "*-Right Norm Key-*"))

		(define (set-left-norms L0 L1 L2 RMS)
			(cog-set-value! wild-atom l-norm-key
				(FloatValue L0 L1 L2 RMS)))

		(define (get-left-support)
			(cog-value-ref (cog-value wild-atom l-norm-key) 0))

		(define (get-left-size)
			(cog-value-ref (cog-value wild-atom l-norm-key) 1))

		(define (get-left-length)
			(cog-value-ref (cog-value wild-atom l-norm-key) 2))

		(define (get-left-rms-length)
			(cog-value-ref (cog-value wild-atom l-norm-key) 3))

		(define (set-right-norms L0 L1 L2 RMS)
			(cog-set-value! wild-atom r-norm-key
				(FloatValue L0 L1 L2 RMS)))

		(define (get-right-support)
			(cog-value-ref (cog-value wild-atom r-norm-key) 0))

		(define (get-right-size)
			(cog-value-ref (cog-value wild-atom r-norm-key) 1))

		(define (get-right-length)
			(cog-value-ref (cog-value wild-atom r-norm-key) 2))

		(define (get-right-rms-length)
			(cog-value-ref (cog-value wild-atom r-norm-key) 3))

		; ----------------------------------------------------
		(define (get-total-count) totcnt)

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
				((left-length)         (get-left-length))
				((right-length)        (get-right-length))
				((left-rms-length)     (get-left-rms-length))
				((right-rms-length)    (get-right-rms-length))
				((set-left-norms)      (apply set-left-norms args))
				((set-right-norms)     (apply set-right-norms args))

				(else (apply llobj (cons message args)))
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
			(frq-obj (add-pair-freq-api wild-obj))
			(rpt-obj (add-report-api wild-obj))
		)

		(define (get-left-fn-avg FN)
			; The Right-METHOD gives the stat on a row that we want
			; to take the weighted average of.  The weight is the
			; probability of that row, which is P(x,*) i.e. right-freq
			; The sum is over all the rows.
			(fold
				(lambda (sum item)
					(+ sum (*
							(FN item)
							(frq-obj 'right-wild-freq item))))
				0
				(wild-obj 'left-basis)))

		(define (do-get-left-avg R-METHOD)
			(get-left-fn-avg (lambda (x) (len-obj R-METHOD x))))

		(define (get-right-fn-avg FN)
			; The Left-METHOD gives the stat on a column that we want
			; to take the weighted average of.  The weight is the
			; probability of that column, which is P(*,y) i.e. left-freq
			; The sum is over all the columns.
			(fold
				(lambda (sum item)
					(+ sum (*
							(FN item)
							(frq-obj 'left-wild-freq item))))
				0
				(wild-obj 'right-basis)))

		(define (do-get-right-avg L-METHOD)
			(get-right-fn-avg (lambda (x) (len-obj L-METHOD x))))

		; ---------------------
		(define (get-left-support) (do-get-left-avg 'right-support))

		(define (get-right-support) (do-get-right-avg 'left-support))

		; ---------------------
		(define (get-left-size) (do-get-left-avg 'right-count))

		(define (get-right-size) (do-get-right-avg 'left-count))

		; ---------------------
		(define (get-left-length) (do-get-left-avg 'right-length))

		(define (get-right-length) (do-get-right-avg 'left-length))

		; ---------
		(define (get-left-rms-length)
			(get-left-fn-avg
				(lambda (x)
					(define len (len-obj 'right-length x))
					(define siz (len-obj 'right-count x))
					(sqrt (- (* len len) (* siz siz))))))

		(define (get-right-rms-length)
			(get-right-fn-avg
				(lambda (x)
					(define len (len-obj 'left-length x))
					(define siz (len-obj 'left-count x))
					(sqrt (- (* len len) (* siz siz))))))

		; ----------------------------------------------------
		; Compute and cache the values of the computation with the
		; report-api can find them.

		(define (cache-left-norms)
			(rpt-obj 'set-left-norms
				(get-left-support)
				(get-left-size)
				(get-left-length)
				(get-left-rms-length)))

		(define (cache-right-norms)
			(rpt-obj 'set-right-norms
				(get-right-support)
				(get-right-size)
				(get-right-length)
				(get-right-rms-length)))

		; ----------------------------------------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-support)      (get-left-support))
				((right-support)     (get-right-support))
				((left-size)         (get-left-size))
				((right-size)        (get-right-size))
				((left-length)       (get-left-length))
				((right-length)      (get-right-length))
				((left-rms-length)   (get-left-rms-length))
				((right-rms-length)  (get-right-rms-length))
				((cache-left-norms)  (cache-left-norms))
				((cache-right-norms) (cache-right-norms))

				(else (apply llobj (cons message args)))
			))
	)
)

; ---------------------------------------------------------------------

(define (print-support-summary-report LLOBJ PORT)

	(define rpt-obj (add-report-api LLOBJ))
	(define fail-before-print (rpt-obj 'left-support))

	(format PORT "\n")
	(format PORT "               Left   Right\n")
	(format PORT "               ----   -----\n")
	(format PORT "Support (l_0)  ~6f    ~6f\n"
		(rpt-obj 'left-support) (rpt-obj 'right-support))
	(format PORT "Size    (l_1)  ~6f    ~6f\n"
		(rpt-obj 'left-size) (rpt-obj 'right-size))
	(format PORT "Length  (l_2)  ~6f    ~6f\n"
		(rpt-obj 'left-length) (rpt-obj 'right-length))
	(format PORT "RMS Length     ~6f    ~6f\n"
		(rpt-obj 'left-rms-length) (rpt-obj 'right-rms-length))
)

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

	(catch #t
		(lambda () (print-support-summary-report LLOBJ PORT))
		(lambda (key . args)
			(format PORT "No support statistics are present\n")
			#f))
)

; ---------------------------------------------------------------------
