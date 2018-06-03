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
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog))

; ---------------------------------------------------------------------

(define*-public (add-report-api LLOBJ
   #:optional (ID (LLOBJ 'id)))
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

  The optional ID argument should be #f or a string, used to construct
  the key under which the values are stored.

  The implemented methods return the following values:
  'left-dim         -- The number of rows
  'right-dim        -- The number of columns
  'num-pairs        -- The number of non-zero entries
  'total-count      -- Total number of observations on all pairs
                       (Identical to the 'wild-wild-count on the
                       count-api object)

  'left-entropy     -- The sum H_left = -sum_x P(x,*) log_2 P(x,*)
  'right-entropy    -- The sum H_right = -sum_y P(*,y) log_2 P(*,y)
  'total-entropy    -- The sum H_tot = sum_x sum_y P(x,y) log_2 P(x,y)
  'total-mi         -- The sum MI = H_tot - H_left - H_right

  'left-support     -- average l_0 norm of rows
  'left-count       -- average l_1 norm of rows
  'left-length      -- average l_2 norm of rows
  'left-rms-count   -- standard deviation of counts

  'right-support    -- average l_0 norm of columns
  'right-count      -- average l_1 norm of columns
  'right-length     -- average l_2 norm of columns
  'right-rms-count -- standard deviation of counts

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
        left-count = sum_x P(x,*) N(x,*)
        left-length = sqrt [ sum_x P(x,*) L(x,*) ]
        left-rms-count = sqrt [ sum_x P(x,*)
               [ L^2(x,*) - (N(x,*))^2 / |(x*)|| ] ]

    Note that while computing the average length of a row/column,
    this is weighted by the probability of that row/column.

    The rms-count computation may seem squonky: to get the correct
    rms, the 'mean' must be taken, by dividing by the support. But
    after this is done, support is multiplied back in.  The point
    here is that count is not the average-count, and the length
    is not divided by teh support either.  So the rms-count should
    avoid an accidental divide by the support; thus the slightly
    odd-looking formula above.

    The rms-count is the square root of what R. Ferrer i Cancho
    calls 'hubbiness' (his hubbiness is the 2nd central moment, if
    I recall correctly).
"
	(let* ((cntobj (add-pair-count-api LLOBJ))
			(totcnt (cntobj 'wild-wild-count))
			(wild-atom (LLOBJ 'wild-wild))
			(is-filtered? (and ID (LLOBJ 'filters?)))
		)

		; ----------------------------------------------------
		; Key under which the matrix dimensions are stored.
		(define dim-key (PredicateNode
			(if is-filtered?
				(string-append "*-Dimension Key " ID)
				"*-Dimension Key-*")))

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
		(define ent-key (PredicateNode
			(if is-filtered?
				(string-append "*-Total Entropy Key " ID)
				"*-Total Entropy Key-*")))

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
		(define mi-key (PredicateNode
			(if is-filtered?
				(string-append "*-Total MI Key " ID)
				"*-Total MI Key-*")))

		(define (set-mi TOT)
			(cog-set-value! wild-atom mi-key (FloatValue TOT)))

		(define (get-total-mi)
			(cog-value-ref (cog-value wild-atom mi-key) 0))

		; ----------------------------------------------------
		; Key under which the matrix l_p norms are stored.
		(define l-norm-key (PredicateNode
			(if is-filtered?
				(string-append "*-Left Norm Key " ID)
				"*-Left Norm Key-*")))
		(define r-norm-key (PredicateNode
			(if is-filtered?
				(string-append "*-Right Norm Key " ID)
				"*-Right Norm Key-*")))

		(define (set-left-norms L0 L1 L2 RMS)
			(cog-set-value! wild-atom l-norm-key
				(FloatValue L0 L1 L2 RMS)))

		(define (get-left-support)
			(cog-value-ref (cog-value wild-atom l-norm-key) 0))

		(define (get-left-count)
			(cog-value-ref (cog-value wild-atom l-norm-key) 1))

		(define (get-left-length)
			(cog-value-ref (cog-value wild-atom l-norm-key) 2))

		(define (get-left-rms-count)
			(cog-value-ref (cog-value wild-atom l-norm-key) 3))

		(define (set-right-norms L0 L1 L2 RMS)
			(cog-set-value! wild-atom r-norm-key
				(FloatValue L0 L1 L2 RMS)))

		(define (get-right-support)
			(cog-value-ref (cog-value wild-atom r-norm-key) 0))

		(define (get-right-count)
			(cog-value-ref (cog-value wild-atom r-norm-key) 1))

		(define (get-right-length)
			(cog-value-ref (cog-value wild-atom r-norm-key) 2))

		(define (get-right-rms-count)
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
				((left-count)          (get-left-count))
				((right-count)         (get-right-count))
				((left-length)         (get-left-length))
				((right-length)        (get-right-length))
				((left-rms-count)      (get-left-rms-count))
				((right-rms-count)     (get-right-rms-count))
				((set-left-norms)      (apply set-left-norms args))
				((set-right-norms)     (apply set-right-norms args))

				(else                  (apply LLOBJ (cons message args)))
			))
	)
)

; ---------------------------------------------------------------------

(define-public (make-central-compute LLOBJ)
"
  add-central-compute LLOBJ - Extend LLOBJ with methods to compute
  misc graph-centrality statistics.
"
	(let* ((wild-obj (add-pair-stars LLOBJ))
			(len-obj (add-support-api wild-obj))
			(frq-obj (add-pair-freq-api wild-obj))
			(rpt-obj (add-report-api wild-obj))
		)

		(define (get-left-fn-avg FN)
			; The Right-METHOD gives the stat on a row that we want
			; to take the weighted average of.  The weight is the
			; probability of that row, which is P(x,*) i.e. right-freq
			; The sum is over all the rows.
			(fold
				(lambda (item sum)
					(+ sum (*
							(FN item)
							(frq-obj 'right-wild-freq item))))
				0.0
				(wild-obj 'left-basis)))

		(define (do-get-left-avg R-METHOD)
			(get-left-fn-avg (lambda (x) (len-obj R-METHOD x))))

		(define (get-right-fn-avg FN)
			; The Left-METHOD gives the stat on a column that we want
			; to take the weighted average of.  The weight is the
			; probability of that column, which is P(*,y) i.e. left-freq
			; The sum is over all the columns.
			(fold
				(lambda (item sum)
					(+ sum (*
							(FN item)
							(frq-obj 'left-wild-freq item))))
				0.0
				(wild-obj 'right-basis)))

		(define (do-get-right-avg L-METHOD)
			(get-right-fn-avg (lambda (x) (len-obj L-METHOD x))))

		; ---------------------
		(define (get-left-support) (do-get-left-avg 'right-support))

		(define (get-right-support) (do-get-right-avg 'left-support))

		; ---------------------
		(define (get-left-count) (do-get-left-avg 'right-count))

		(define (get-right-count) (do-get-right-avg 'left-count))

		; ---------------------
		(define (get-left-length) (do-get-left-avg 'right-length))

		(define (get-right-length) (do-get-right-avg 'left-length))

		; ---------
		; XXX FIXME. This is totally insane, but guile sometimes
		; returns a small imaginary part for the fold, even though
		; each and every term, and each and every partial sum had
		; no imaginary part on it at all! WTF! But then we get to
		; here, and it does!! So we take the real part, else SQL
		; chokes on the imaginary value.
		;
		; Note - the divide by the support can be understood as follows:
		; avg = len / sup
		; mean-sq = (len * len) / sup
		; variance = mean-sq - avg * avg
		; moment = sup * variance
		; rms-count = sqrt (moment)
		; multiple through by sup to get the implementation below.
		(define (get-left-rms-count)
			(real-part
			(get-left-fn-avg
				(lambda (x)
					(define sup (len-obj 'right-support x))
					(define siz (len-obj 'right-count x))
					(define len (len-obj 'right-length x))
					(define lensq (* len len))
					(define sizsq (/ (* siz siz) sup))
					(sqrt (- lensq sizsq))))))

		(define (get-right-rms-count)
			(real-part
			(get-right-fn-avg
				(lambda (x)
					(define sup (len-obj 'left-support x))
					(define siz (len-obj 'left-count x))
					(define len (len-obj 'left-length x))
					(define lensq (* len len))
					(define sizsq (/ (* siz siz) sup))
					(sqrt (- lensq sizsq))))))

		; ----------------------------------------------------
		; Compute and cache the values of the computation with the
		; report-api can find them.

		(define (cache-all)
			(define start-time (current-time))
			(define (elapsed-secs)
				(define diff (- (current-time) start-time))
				(set! start-time (current-time))
				diff)

			(rpt-obj 'set-left-norms
				(get-left-support)
				(get-left-count)
				(get-left-length)
				(get-left-rms-count))

			(format #t "Finished left norm totals in ~A secs\n"
				(elapsed-secs))

			(rpt-obj 'set-right-norms
				(get-right-support)
				(get-right-count)
				(get-right-length)
				(get-right-rms-count))

			(format #t "Finished right norm totals in ~A secs\n"
				(elapsed-secs))
		)


		; ----------------------------------------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-support)      (get-left-support))
				((right-support)     (get-right-support))
				((left-count)        (get-left-count))
				((right-count)       (get-right-count))
				((left-length)       (get-left-length))
				((right-length)      (get-right-length))
				((left-rms-count)    (get-left-rms-count))
				((right-rms-count)   (get-right-rms-count))
				((cache-all)         (cache-all))

				(else                (apply LLOBJ (cons message args)))
			))
	)
)

; ---------------------------------------------------------------------

(define (print-entropy-summary-report LLOBJ PORT)

	(define rpt-obj (add-report-api LLOBJ))

	(format PORT "Entropy Total: ~6f   Left: ~6f   Right: ~6f\n"
		(rpt-obj 'total-entropy)
		(rpt-obj 'left-entropy)
		(rpt-obj 'right-entropy)
	)
	(format PORT "Total MI: ~6f\n" (rpt-obj 'total-mi))
)

(define (print-support-summary-report LLOBJ PORT)

	(define rpt-obj (add-report-api LLOBJ))
	(define ls (rpt-obj 'left-support))
	(define rs (rpt-obj 'right-support))
	(define lc (rpt-obj 'left-count))
	(define rc (rpt-obj 'right-count))
	(define ll (rpt-obj 'left-length))
	(define rl (rpt-obj 'right-length))
	(define lv (rpt-obj 'left-rms-count))
	(define rv (rpt-obj 'right-rms-count))

	(format PORT "\n")
	(format PORT "                 Left         Right     Avg-left     Avg-right\n")
	(format PORT "                 ----         -----     --------     ---------\n")
	(format PORT "Support (l_0)  ~9,4g    ~9,4g\n"  ls  rs)
	(format PORT "Count   (l_1)  ~9,4g    ~9,4g     ~9,4g    ~9,4g\n"
		lc rc (/ lc ls) (/ rc rs))
	(format PORT "Length  (l_2)  ~9,4g    ~9,4g     ~9,4g    ~9,4g\n"
		ll rl (/ ll ls) (/ rl rs))
	(format PORT "RMS Count      ~9,4g    ~9,4g     ~9,4g    ~9,4g\n"
		lv rv (/ lv ls) (/ rv rs))
)

(define*-public (print-matrix-summary-report LLOBJ
	#:optional (PORT #t))
"
  print-matrix-summary-report LLOBJ #:optional PORT
  Print a summary report about the pair dataset LLOBJ to the
  optionally-provided output PORT (e.g. a string or file port).

  See documentation for `add-report-api` for an explanation of
  what is being printed.
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

	(catch #t
		(lambda () (print-entropy-summary-report LLOBJ PORT))
		(lambda (key . args)
			(format PORT
				"No MI statistics are present; run compute-mi to get them.\n")
			#f))

	(catch #t
		(lambda () (print-support-summary-report LLOBJ PORT))
		(lambda (key . args)
			(format PORT
				"No support statistics are present. Run compute-mi to get them.\n")
			#f))
)

; ---------------------------------------------------------------------
