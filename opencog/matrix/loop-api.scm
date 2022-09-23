;
; loop-api.scm
;
; Loop over all pairs of items in a sparse matrix.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Sometimes, one needs to loop over all matrix entries. This is not
; hard to do, and most users can easily do this. However, if you are
; lazy, and prefer reading docs, then the below provides two methods
; to visit every non-zero entry in a sparse matrix.
;
; The matrix is accessed using the wild-card stars object, and
; specifically uses these methods:
;   'get-pair, which should return high-level pair, given the
;        low-level pair.
;   'left-basis and 'right-basis, providing a list of all rows and columns
;   'left-stars and 'right-stars, providing a list of all non-zero
;        entries in each row and column.

; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to loop over all pairs
; in a matrix.
;
(define-public (add-loop-api LLOBJ)

	; We need 'left-basis, provided by add-pair-stars
	(let ((wldobj (add-pair-stars LLOBJ)))

		; This calls f(x,y) for all (x,y)
		; Outer loop is the loop over rows; inner loop is over columns
		(define (foreach-right-outer FUNC)

			; The outer-loop.
			(define (right-loop left-item)
				(for-each FUNC
					(wldobj 'right-stars left-item)))

			(for-each right-loop (wldobj 'left-basis))
		)

		; This calls f(x,y) for all (x,y), and returns a list of results.
		; Outer loop is the loop over rows; inner loop is over columns
		(define (map-right-outer FUNC)

			(define rtn '())

			; The outer-loop.
			(define (right-loop left-item)
				(for-each
					(lambda (pr)
						(set! rtn (cons (FUNC pr) rtn)))
					(wldobj 'right-stars left-item)))

			(for-each right-loop (wldobj 'left-basis))
			rtn
		)

		; Methods on this class.
		(lambda (message . args)
			(case message
				((for-each-pair)  (apply foreach-right-outer args))
				((map-pair)       (apply map-right-outer args))

				(else             (apply LLOBJ (cons message args))))
		))
)

; ---------------------------------------------------------------------

(define-public (loop-upper-diagonal LLOBJ FUN ITEMLI START-RANK DEPTH)
"
  loop-upper-diagonal LLOBJ FUN ITEMLI START-RANK DEPTH - call FUN on pairs.

  This implements a loop that calls FUN on pairs of items, chosen from
  ITEMLI, lying near the diagonal.  The width of the diagonal is DEPTH.
  The diagonal is defined by items in ITEMLI, which is taken to be a
  ranked list.  Computations start at START-RANK and proceed to DEPTH.

  Think of a tri-diagonal matrix, but instead of three, it is N-diagonal,
  with N given by DEPTH. It is assumed that the matrix is symmetric,
  i.e. that `(FUN x y)` equals `(FUN y x)` and so only the upper-
  triangular part is computed.

  Examples: If START-RANK is 0 and DEPTH is 200, then the 200x200
  block matrix of pairs will be computed. Since the function FUN is
  symmetric, this is a symmetric matrix, and so 200 x 201 / 2 grand
  total pairs are computed. (This is a 'triangle number')

  If START-RANK is 300 and DEPTH is 200, then computations start at
  the 300'th ranked item and continue through the 500'th ranked item.
  This results in a total of 200x200 pairs, as 200 rows are
  computed, out to 200 places away from the diagonal. Visually, this is
  a rhombus, one side lying along the diagonal (a rhombus is a
  parellelogram with all sides equal length.)

  Here's an ascii-art image for the pairs computed, out to DEPTH=3 for
  a list of 7 items:

         @ + +
         - @ + +
         - - @ + +
           - - @ + +
             - - @ + +
               - - @ +
                 - - @

   The @ marks the diagonal, and the + and - mark the off-diagonal
   entries that will be computed. Because FUN is assumed to be symmtric,
   it is called only on the diagonals and the + entries.
"
	; Perform pair computations for one row.
	(define (batch-simlist ITEM ITEM-LIST)
		(for-each
			(lambda (item) (FUN ITEM item))
			ITEM-LIST))

	; Take the item list and trim it down.
	(define nitems (length ITEMLI))
	(define start (min START-RANK nitems))   ; avoid overflow
	(define depth (min DEPTH (- nitems start)))  ; avoid overflow
	(define row-range (take (drop ITEMLI start) depth)) ; list of items to do
	(define (col-start off) (max 0 (- (+ start off) depth))) ;  column start
	(define (col-end off) (min (+ start off) depth)) ;  column end
	(define (col-range off)   ; reverse, so we go from diagonal outwards
		(reverse (take (drop ITEMLI (col-start off)) (col-end off))))

	(define (do-one-row off)
		(define pone (+ 1 off))
		(batch-simlist (list-ref row-range off) (col-range pone)))

	(define rpt-one-row
		(make-progress-rpt do-one-row 10 #f
			"Diag: Finished ~D rows in ~D secs (~5F/sec)\n"
			60))

	; Perform the similarity calculations, looping over the fat diagonal.
	(for-each (lambda (n) (rpt-one-row n)) (iota depth))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
