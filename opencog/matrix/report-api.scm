;
; report-api.scm
;
; Define API providing overview of the correlation matrix.
;
; Copyright (c) 2017 Linas Vepstas

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

  It is presumed that the entropies have been previously computed,
  with the `make-compute-freq`, the `make-all-pair-mi` and the
  `make-central-compute` objects.

  The optional ID argument should be #f or a string, used to construct
  the key under which the values are stored.

  The implemented methods return the following values:
  'left-dim         -- The number of rows
  'right-dim        -- The number of columns
  'num-pairs        -- The number of non-zero entries
  'total-count      -- Total number of observations on all pairs
                       (Identical to the 'wild-wild-count on the
                       support-api object)

  'left-entropy     -- The sum H_left = -sum_y P(*,y) log_2 P(*,y)
  'right-entropy    -- The sum H_right = -sum_x P(x,*) log_2 P(x,*)
  'total-entropy    -- The sum H_tot = sum_x sum_y P(x,y) log_2 P(x,y)
  'total-mi         -- The sum MI = H_tot - H_left - H_right

  The averages below are weighted-averages, so that, for example,
  the 'left-support is the average, taken over all columns, of the
  support of each column (i.e. is the average of all 'left-supports).
  The weight is the probability of that column, i.e. is P(*,y) viz
  it is the 'left-freq or 'left-count/total.

  Note that the `add-support-api` also provides methods with the same
  names. This object provides matrix-wide averages of the support,
  count and length, whereas that object provides per-row/per-column
  values of each.

  'left-support     -- average (over columns) l_0 norm of columns
  'left-count       -- average l_1 norm of columns
  'left-length      -- average l_2 norm of columns
  'left-rms-count   -- standard deviation of counts

  'right-support    -- average l_0 norm of rows
  'right-count      -- average l_1 norm of rows
  'right-length     -- average l_2 norm of rows
  'right-rms-count -- standard deviation of counts

  If we imagine each pair as a directed edge, an arrow pointing from
  right to left, then the left-support is the same as the average
  out-degree of the right-vertexes. The right-support is the average
  in-degree of the left-vertexes. Equivalently, the left-support is
  the average number of non-zero entries in each column, and the
  right-support is the average number of non-zero entries in each
  row.

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
        right-support = sum_x P(x,*) |(x,*)|
        right-count = sum_x P(x,*) N(x,*)
        right-length = sqrt [ sum_x P(x,*) L(x,*) ]
        right-rms-count = sqrt [ sum_x P(x,*)
               [ L^2(x,*) - (N(x,*))^2 / |(x*)|| ] ]

    Note that while computing the average length of a row/column,
    this is weighted by the probability of that row/column.

    The rms-count computation may seem squonky: to get the correct
    rms, the 'mean' must be taken, by dividing by the support. But
    after this is done, support is multiplied back in.  The point
    here is that count is not the average-count, and the length
    is not divided by the support either.  So the rms-count should
    avoid an accidental divide by the support; thus the slightly
    odd-looking formula above.

    The rms-count is the square root of what R. Ferrer i Cancho
    calls 'hubbiness' (his hubbiness is the 2nd central moment, if
    I recall correctly).
"
	(let* ((supobj (add-support-api LLOBJ))
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
		(define (get-total-count) (supobj 'wild-wild-count))

		;-------------------------------------------

		(define (help)
			(format #t
				(string-append
"This is the `add-report-api` object applied to the \"~A\"\n"
"object.  It provides matrix-wide summary values characterizing the\n"
"matrix\n"
"\n"
"For more information, say `,d add-report-api` at the guile prompt,\n"
"or just use the 'describe method on this object. You can also get at\n"
"the base object with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
				(LLOBJ 'id)))

		(define (describe)
			(display (procedure-property add-report-api 'documentation)))

		; ----------------------------------------------------
		; Methods on this class.
		(lambda (message . args)
			(define (oops)
				(throw 'wrong-number-of-args 'add-report-api
					(format #f "The '~A method does not expect any arguments!" message)))

			; Error checking to avoid my own screw-ups.
			(if (< 0 (length args))
				(case message
					((left-dim)            (oops))
					((right-dim)           (oops))
					((num-pairs)           (oops))

					((left-entropy)        (oops))
					((right-entropy)       (oops))
					((total-entropy)       (oops))
					((total-mi)            (oops))

					((total-count)         (oops))
					((left-support)        (oops))
					((right-support)       (oops))
					((left-count)          (oops))
					((right-count)         (oops))
					((left-length)         (oops))
					((right-length)        (oops))
					((left-rms-count)      (oops))
					((right-rms-count)     (oops))
				))

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

				((help)                (help))
				((describe)            (describe))
				((obj)                 "add-report-api")
				((base)                LLOBJ)
				(else                  (apply LLOBJ (cons message args)))
			))
	)
)

; ---------------------------------------------------------------------

(define-public (make-central-compute LLOBJ)
"
  add-central-compute LLOBJ - Extend LLOBJ with methods to compute
  misc graph-centrality statistics. These are reported by the report
  object (by the `add-report-api` object).

  The stats include the weighted-average support, count and length of
  rows and columns. The weighted-average is the frequency-weighted
  average: So, for example, if a row is very long, but is very rarely
  seen, then it will not contribute much to the average. See the
  documentation on `add-report-api` for precise definitions.
"
	(let* ((wild-obj (add-pair-stars LLOBJ))
			(len-obj (add-support-api wild-obj))
			(rpt-obj (add-report-api wild-obj))
		)

		; Take an average over all rows
		(define (get-row-fn-avg FN)
			; The FN function gives the stat on a row that we want
			; to take the weighted average of.  The weight is the
			; probability of that row, which is P(x,*) i.e. right-freq
			; i.e. the right-count divided by the total count.
			; The sum is over all the rows.
			(define weighted-avg (fold
				(lambda (item sum)
					(+ sum (*
							(FN item)
							(len-obj 'right-count item))))
				0.0
				(wild-obj 'left-basis)))

			; TODO - some future day, use 'total-count-right
			; For now, too many existing datasets don't store this value.
			; (define total (len-obj 'total-count-right))
			(define total (len-obj 'wild-wild-count))

			(/ weighted-avg total))

		(define (do-get-row-avg R-METHOD)
			(get-row-fn-avg (lambda (x) (len-obj R-METHOD x))))

		; Take an average over all columns
		(define (get-col-fn-avg FN)
			; The FN function gives the stat on a column that we want
			; to take the weighted average of.  The weight is the
			; probability of that column, which is P(*,y) i.e. left-freq
			; i.e. the left-count divided by the total count.
			; The sum is over all the columns.
			(define weighted-avg (fold
				(lambda (item sum)
					(+ sum (*
							(FN item)
							(len-obj 'left-count item))))
				0.0
				(wild-obj 'right-basis)))

			; TODO - some future day, use 'total-count-left
			; For now, too many existing datasets don't store this value.
			; (define total (len-obj 'total-count-left))
			(define total (len-obj 'wild-wild-count))

			(/ weighted-avg total))

		(define (do-get-col-avg L-METHOD)
			(get-col-fn-avg (lambda (x) (len-obj L-METHOD x))))

		; ---------------------
		; Get the weighted-average support.
		(define (get-right-support) (do-get-row-avg 'right-support))

		(define (get-left-support) (do-get-col-avg 'left-support))

		; ---------------------
		; Get the weighted-average count.
		(define (get-right-count) (do-get-row-avg 'right-count))

		(define (get-left-count) (do-get-col-avg 'left-count))

		; ---------------------
		(define (get-right-length) (do-get-row-avg 'right-length))

		(define (get-left-length) (do-get-col-avg 'left-length))

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
		(define (get-right-rms-count)
			(real-part
			(get-row-fn-avg
				(lambda (x)
					(define sup (len-obj 'right-support x))
					(define siz (len-obj 'right-count x))
					(define len (len-obj 'right-length x))
					(define lensq (* len len))
					(define sizsq (/ (* siz siz) sup))
					(if (finite? sizsq)
						(sqrt (- lensq sizsq)) 0.0)))))

		(define (get-left-rms-count)
			(real-part
			(get-col-fn-avg
				(lambda (x)
					(define sup (len-obj 'left-support x))
					(define siz (len-obj 'left-count x))
					(define len (len-obj 'left-length x))
					(define lensq (* len len))
					(define sizsq (/ (* siz siz) sup))
					(if (finite? sizsq)
						(sqrt (- lensq sizsq)) 0.0)))))

		; ----------------------------------------------------
		; Compute and cache the values of the computation where the
		; report-api can find them.

		(define (cache-total)
			; Note that total-support-left should equal
			; 'total-support-right, up to round-off errors.
			(rpt-obj 'set-size
				(wild-obj 'left-basis-size)
				(wild-obj 'right-basis-size)
				(len-obj 'total-support-left))
		)

		(define (cache-left)
			(define elapsed-secs (make-elapsed-secs))

			; Note that total-support-left should equal
			; 'total-support-right, up to round-off errors.
			(rpt-obj 'set-size
				(wild-obj 'left-basis-size)
				(wild-obj 'right-basis-size)
				(len-obj 'total-support-right))

			(rpt-obj 'set-left-norms
				(get-left-support)
				(get-left-count)
				(get-left-length)
				(get-left-rms-count))

			(format #t "Finished column (left) norm averages in ~A secs\n"
				(elapsed-secs))
		)

		(define (cache-right)
			(define elapsed-secs (make-elapsed-secs))

			; Note that total-support-left should equal
			; 'total-support-right, up to round-off errors.
			(rpt-obj 'set-size
				(wild-obj 'left-basis-size)
				(wild-obj 'right-basis-size)
				(len-obj 'total-support-left))

			(rpt-obj 'set-right-norms
				(get-right-support)
				(get-right-count)
				(get-right-length)
				(get-right-rms-count))

			(format #t "Finished row (right) norm averages in ~A secs\n"
				(elapsed-secs))
		)

		(define (cache-all) (cache-left) (cache-right))

		;-------------------------------------------

		(define (help)
			(format #t
				(string-append
"This is the `make-central-compute` object applied to the \"~A\"\n"
"object.  It provides matrix-wide summary values characterizing the\n"
"matrix\n"
"\n"
"For more information, say `,d make-central-compute` at the guile prompt,\n"
"or just use the 'describe method on this object. You can also get at\n"
"the base object with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
				(LLOBJ 'id)))

		(define (describe)
			(display (procedure-property make-central-compute 'documentation)))

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
				((cache-left)        (cache-left))
				((cache-right)       (cache-right))
				((cache-total)       (cache-total))
				((cache-all)         (cache-all))

				((help)              (help))
				((describe)          (describe))
				((obj)               "make-central-compute")
				((base)              LLOBJ)

				(else                (apply LLOBJ (cons message args)))
			))
	)
)

; ---------------------------------------------------------------------

(define (print-entropy-summary-report LLOBJ PORT)

	(define rpt-obj (add-report-api LLOBJ))

	(format PORT "Entropy Total: ~7f   Left: ~7f   Right: ~7f\n"
		(rpt-obj 'total-entropy)
		(rpt-obj 'left-entropy)
		(rpt-obj 'right-entropy)
	)
	(format PORT "Total MI: ~6f\n" (rpt-obj 'total-mi))
)

(define (print-centrality-summary-report LLOBJ PORT)

	(define rpt-obj (add-report-api LLOBJ))
	(define ls
		(catch #t (lambda () (rpt-obj 'left-support))
		(lambda (key . args) #f)))

	(define rs
		(catch #t (lambda () (rpt-obj 'right-support))
		(lambda (key . args) #f)))

	(define (prt tst val)
		(if tst (format #f "~9,4g" (rpt-obj val)) "   n/a   "))

	(define lc (prt ls 'left-count))
	(define ll (prt ls 'left-length))
	(define lv (prt ls 'left-rms-count))

	(define rc (prt rs 'right-count))
	(define rl (prt rs 'right-length))
	(define rv (prt rs 'right-rms-count))

	(define (avg tst val)
		(if tst (format #f "~9,4g" (/ (rpt-obj val) tst)) "   n/a   "))

	(define alc (avg ls 'left-count))
	(define all (avg ls 'left-length))
	(define alv (avg ls 'left-rms-count))

	(define arc (avg rs 'right-count))
	(define arl (avg rs 'right-length))
	(define arv (avg rs 'right-rms-count))

	(define lss (prt ls 'left-support))
	(define rss (prt rs 'right-support))

	; Print nothing, if neither rows nor columns available
	(if (not (or ls rs))
		(format PORT "No support statistics are present;\n   run `((make-central-compute LLOBJ) 'cache-all)` to get them.\n")
		(begin

	(format PORT "\n")
	(format PORT "                 Left         Right     Avg-left     Avg-right\n")
	(format PORT "                 ----         -----     --------     ---------\n")
	(format PORT "Support (l_0)  ~A    ~A\n"  lss  rss)
	(format PORT "Count   (l_1)  ~A    ~A     ~A    ~A\n" lc rc alc arc)
	(format PORT "Length  (l_2)  ~A    ~A     ~A    ~A\n" ll rl all arl)
	(format PORT "RMS Count      ~A    ~A     ~A    ~A\n" lv rv alv arv)
)))

(define (print-transpose-summary-report LLOBJ PORT)

	(define trans-obj (add-transpose-api LLOBJ))

	(define mmt-support (trans-obj 'total-mmt-support))
	(define mmt-count (trans-obj 'total-mmt-count))
	(define mtm-support (trans-obj 'total-mtm-support))
	(define mtm-count (trans-obj 'total-mtm-count))

	; This is -log_2 (sum_d N(*,d) N(*,d)) / N(*,*) N(*,*)
	; See diary for more info.
	(define mmt-entropy 0)
	(define mtm-entropy 0)
	(if (< 0 mmt-support)
		(set! mmt-entropy
			(/ (- (log (/ mmt-count (* mmt-support mmt-support)))) (log 2))))

	(if (< 0 mtm-support)
		(set! mtm-entropy
			(/ (- (log (/ mtm-count (* mtm-support mtm-support)))) (log 2))))

	(format PORT "\n")
	(if (< 0 mmt-support)
		(format PORT "MM^T support=~6g count=~6g entropy=~6f\n"
			mmt-support mmt-count mmt-entropy)
		(format PORT "No MM^T data present\n"))

	(if (< 0 mtm-support)
		(format PORT "M^TM support=~6g count=~6g entropy=~6f\n"
			mtm-support mtm-count mtm-entropy)
		(format PORT "No M^TM data present\n"))
)

(define (print-basic-summary-report LLOBJ PORT)
"
  Print only the most basic report. Requires that the support
  was previously computed, with ((add-support-compute LLOBJ) 'cache-all)
"
	(define (log2 x) (/ (log x) (log 2)))

	(define sup-obj (add-support-api LLOBJ))

	; (sup-obj 'left-dim) is exactly the same as (LLOBJ 'left-basis-size)
	; but is much much faster, because the cached marginal value is used.
	; On large datasets, triggering (LLOBJ 'left-basis-size) can take
	; tens of minutes and many gigabytes of RAM. Downside is that we
	; won't print the dimensions, or the sparsify, if the cached values
	; are not present.
	(let* ((nrows (sup-obj 'left-dim))
			(ncols (sup-obj 'right-dim))
			(tot (* nrows ncols))
			(lsize (sup-obj 'total-support-left))
			(rsize (sup-obj 'total-support-right))
			(nlsize (inexact->exact (round lsize)))
			(nrsize (inexact->exact (round rsize)))
			(lobs (sup-obj 'total-count-left))
			(robs (sup-obj 'total-count-right))
			(nlobs (inexact->exact (round lobs)))
			(nrobs (inexact->exact (round robs))))

		; lsize should equal rsize should equal (sup-obj 'num-pairs)
		; should equal (length (LLOBJ 'get-all-pairs)).
		(if (not (equal? nlsize nrsize))
			(format PORT "Error: left and right total pairs not equal! ~A ~A\n"
				lsize rsize))

		; lobs should equal robs should equal (sup-obj 'wild-wild-count)
		(if (not (equal? nlobs nrobs))
			(format PORT "Error: left and right total counts not equal! ~A ~A\n"
				lobs robs))

		(format PORT "Rows: ~d Columns: ~d  == log_2 ~7f x ~7f\n"
			nrows ncols (log2 nrows) (log2 ncols))
		(format PORT "Size: ~d  log_2 size: ~7f\n"
			lsize (log2 lsize))
		(format PORT "Fraction non-zero: ~9,4g Sparsity: ~7f  Rarity: ~7f\n"
			(/ lsize tot) (log2 (/ tot lsize)) (log2 (/ lsize (sqrt tot))))
		(format PORT "Total obs: ~10f  Avg obs/pair: ~7f  log_2 avg: ~7f\n"
			lobs (/ lobs lsize) (log2 (/ lobs lsize)))
	)
)

(define*-public (print-matrix-summary-report LLOBJ
	#:optional (PORT #t))
"
  print-matrix-summary-report LLOBJ #:optional PORT
  Print a summary report about the pair dataset LLOBJ to the
  optionally-provided output PORT (e.g. a string or file port).

  See documentation for `add-report-api` for an explanation of
  what is being printed.

  All information printed by the report is cached on the wild-card
  atom.  Thus, for quick peeks into datasets residing on disk, it
  can be convenient to say `(fetch-atom (LLOBJ 'wild-wild))` and
  then printing the report. Careful, though: this fetch may clobber
  recently recomputed data that has not yet been stored!
"

	; All data needed for this report is hanging off of just one Atom.
	; Make sure that Atom is in memory. Uhh no, because this may clobber
	; the content that is in RAM!  Youch!
	; (fetch-atom (LLOBJ 'wild-wild))

	(format PORT "Summary Report for Correlation Matrix ~A\n"
		(LLOBJ 'name))
	(format PORT "Left type: ~A    Right Type: ~A    Pair Type: ~A\n"
		(LLOBJ 'left-type) (LLOBJ 'right-type) (LLOBJ 'pair-type))
	(format PORT "Wildcard: ~A" (LLOBJ 'wild-wild))

	(catch #t
		(lambda () (print-basic-summary-report LLOBJ PORT))
		(lambda (key . args)
			(format PORT
				"No cached matrix data available;\n  run ((add-support-compute LLOBJ) 'cache-all) to make one.\n")
			#f))

	(print-centrality-summary-report LLOBJ PORT)
	(print-transpose-summary-report LLOBJ PORT)

	(catch #t
		(lambda () (print-entropy-summary-report LLOBJ PORT))
		(lambda (key . args)
			(format PORT
				"No MI statistics are present; run compute-mi to get them.\n")
			#f))

	*unspecified*
)

; ---------------------------------------------------------------------
