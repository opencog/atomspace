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
; Each matrix has a total entropy sum_x sum_y p(x,y) log_2 p(x,y)
; this is returned by
;
; Each matrix has

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
"
	(let ((llobj LLOBJ)
			(wild-atom (LLOBJ 'wild-wild)))

		; ----------------------------------------------------
		; Key under which the matrix dimensions are stored.
		(define dim-key (PredicateNode "*-Dimension Key-*"))

		(define (set-size LEFT RIGHT NPAIRS)
			(cog-set-value! wild-atom dim-key (FloatValue LEFT RIGHT NPAIRS)))

		(define (get-left-dim)
			(cog-value-ref (cog-value wild-atom dim-key) 0))

		(define (get-right-dim)
			(cog-value-ref (cog-value wild-atom dim-key) 1))

		(define (get-num-pairs)
			(cog-value-ref (cog-value wild-atom dim-key) 2))

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

	(format PORT "Summary Report for Correlation Matrix ~A\n"
		(LLOBJ 'name))
	(format PORT "Left type: ~A    Right Type: ~A    Pair Type: ~A\n"
		(LLOBJ 'left-type) (LLOBJ 'right-type) (LLOBJ 'pair-type))
	(format PORT "Wildcard: ~A\n" (LLOBJ 'wild-wild))

	(format PORT "Rows: ~d Columns: ~d\n"
		(rpt-obj 'left-dim) (rpt-obj 'right-dim))

	(let ((size (rpt-obj 'num-pairs))
			(tot (* (rpt-obj 'left-dim) (rpt-obj 'right-dim)))
			(obs (cnt-obj 'wild-wild-count))
		)
		(format PORT "Size: ~d of ~d  Fraction: ~9,4g Sparsity: ~6f\n"
			size tot (/ size tot) (log2 (/ tot size)))
		(format PORT "Total observations: ~d  Avg obs per pair: ~6f\n"
			obs (/ obs size))
	)

	(format PORT "Left  Entropy: ~6f\n" (rpt-obj 'left-entropy))
	(format PORT "Right Entropy: ~6f\n" (rpt-obj 'right-entropy))
	(format PORT "Total Entropy: ~6f\n" (rpt-obj 'total-entropy))
	(format PORT "Total MI: ~6f\n" (rpt-obj 'total-mi))
)

; ---------------------------------------------------------------------
