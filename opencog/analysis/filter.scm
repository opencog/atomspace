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
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define*-public (add-subtotal-filter LLOBJ LEFT-CUT RIGHT-CUT)
"
  add-subtotal-filter LLOBJ - Modify LLOBJ so that any columns and
  rows with counts less than LEFT-CUT and RIGHT-CUT are removed. This
  provides an API compatible with the star-object API; i.e. it provides
  the same row and column addressability that star-object does, but
  just returns fewer rows and columns.

  Thhe filtering is done "on demand", on a row-by-row, column-by-column
  basis.

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  Let N(*,y) be the column subtotals, AKA the left-subtotals.
  Let N(x,*) be the row subtotals, AKA the right subtotals.

  This object removes all columns where  N(*,y) <= LEFT-CUT and where
  N(x,*) <= RIGHT-CUT.
"
	(let ((llobj LLOBJ)
			)

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((filter)    (apply compute-left-product args))
				(else            (apply llobj (cons message args))))
		)))

; ---------------------------------------------------------------------
