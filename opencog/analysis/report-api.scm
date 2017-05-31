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
  entropies and mutual information.

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

		(define (set-mi LEFT RIGHT TOT)
			(cog-set-value! wild-atom mi-key (FloatValue LEFT RIGHT TOT)))

		(define (get-left-mi)
			(cog-value-ref (cog-value wild-atom mi-key) 0))

		(define (get-right-mi)
			(cog-value-ref (cog-value wild-atom mi-key) 1))

		(define (get-total-mi)
			(cog-value-ref (cog-value wild-atom mi-key) 2))

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

				((left-mi)             (get-left-mi))
				((right-mi)            (get-right-mi))
				((total-mi)            (get-total-mi))
				((set-mi)              (apply set-mi args))
			))
	)
)

; ---------------------------------------------------------------------
