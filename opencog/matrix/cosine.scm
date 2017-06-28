;
; cosine.scm
;
; Define API for computing the cosine and jaccard distances between two
; rows or columns of a sparse matrix.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; See object-api.scm for the overview of how sparse matrixes are defined.
; Or see the README.md file.
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define*-public (add-pair-cosine-compute LLOBJ
	#:optional (GET-CNT 'pair-count))
"
  add-pair-cosine-compute LLOBJ - Extend LLOBJ with methods to compute
  vector dot-products, cosine angles and jaccard distances between two
  rows or columns of the LLOBJ sparse matrix.

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  There are two ways of computing a dot-product: summing on the left,
  to get a dot-product of columns, or summing on the right, to get a
  product of rows.  Thus we define the left-product as
      left-prod(y,z) = sum_x N(x,y) N(x,z)
  where y and z are two different column indexes.  Likewise, the right
  product is
      right-prod(x,u) = sum_y N(x,y) N(u,y)
  with x and u being two different row indexes.

  Cosines:
  --------
  Similarly, we can define the left and right cosine angles as
      left-cosine(y,z) = left-prod(y,z) /
             (left-length(y) * left-length(z))
  and likewise for the right side. Recall that the left-length is
  defined as
      left-length(y) = sqrt sum_x N(x,y) N(x,y)
                     = sqrt left-prod(y,y)

  Jaccard:
  --------
  The Jaccard similarity can be defined as one minus the Jaccard
  similarity, which is defined as

      left-jacc-sim(y,z) = sum_x min (N(x,y), N(x,z)) /
               sum_x max (N(x,y), N(x,z))

  Overlap:
  --------
  The overlap similarity simply counts how many common non-zero entries
  are shared in common between two rows or columns.  That is, it is

      left-overlap(y,z) = sum_x (0 < N(x,y)) * (0 < N(x,z)) /
               sum_x (0 < N(x,y) + N(x,z))

  Arguments:
  ----------
  Here, the LLOBJ is expected to be an object defining a sparse matrix,
  with valid counts associated with each pair. LLOBJ is expected to have
  working, functional methods for 'left-type, 'right-type and 'pair-type
  on it.

  By default, the N(x,y) is taken to be the 'get-count method on LLOBJ,
  i.e. it is literally the count. The optional argument GET-CNT allows
  this to be over-ridden with any other method that returns a number.
  For example, to compute the products and cosines for frequencies, pass
  'pair-freq as the second argument.  Any method that takes a matrix
  element pair and returns a number is allowed.
"

	(define (either x y) (if (or (< 0.0 x) (< 0.0 y)) 1.0 0.0))
	(define (both x y) (if (and (< 0.0 x) (< 0.0 y)) 1.0 0.0))
	(let* ((star-obj (add-pair-stars LLOBJ))
			(supp-obj  (add-support-compute star-obj GET-CNT))
			(prod-obj  (add-support-compute
				(add-tuple-math star-obj * GET-CNT)))
			(min-obj   (add-support-compute
				(add-tuple-math star-obj min GET-CNT)))
			(max-obj   (add-support-compute
				(add-tuple-math star-obj max GET-CNT)))
			(either-obj   (add-support-compute
				(add-tuple-math star-obj either GET-CNT)))
			(both-obj   (add-support-compute
				(add-tuple-math star-obj both GET-CNT)))
		)

		; -------------
		; Return the vector product of column A and column B
		; The prod-obj takes the product of pairs of atrix entries,
		; and the 'left-count method just adds them up.  Equivalently,
		; we could just sum over the left-stars ourselves, but this
		; would take three lines of code instead of one.
		(define (compute-left-product COL-A COL-B)
			(prod-obj 'left-count (list COL-A COL-B)))

		; Return the vector product of row A and row B
		(define (compute-right-product ROW-A ROW-B)
			(prod-obj 'right-count (list ROW-A ROW-B)))

		; -------------
		(define (do-get-left-length COL) (supp-obj 'left-length COL))
		(define get-left-length (make-afunc-cache do-get-left-length))
		(define (do-get-right-length ROW) (supp-obj 'right-length ROW))
		(define get-right-length (make-afunc-cache do-get-right-length))

		; Return the cosine of the angle between column A and B.
		; The cosine as defined above (the usual textbook definition).
		(define (compute-left-cosine COL-A COL-B)
			(define prod (compute-left-product COL-A COL-B))
			(define deno (*
				(get-left-length COL-A)
				(get-left-length COL-B)))
			(if (eqv? 0.0 deno) 0.0 (/ prod deno)))

		; As above, but for the rows.
		(define (compute-right-cosine ROW-A ROW-B)
			(define prod (compute-right-product ROW-A ROW-B))
			(define deno (*
				(get-right-length ROW-A)
				(get-right-length ROW-B)))
			(if (eqv? 0.0 deno) 0.0 (/ prod deno)))

		; -------------
		; Return the left-jaccard distance
		(define (compute-left-jaccard-dist COL-A COL-B)
			(define left-min (min-obj 'left-count (list COL-A COL-B)))
			(define left-max (max-obj 'left-count (list COL-A COL-B)))
			(- 1.0 (/ left-min left-max))
		)

		; Return the right-jaccard distance
		(define (compute-right-jaccard-dist ROW-A ROW-B)
			(define right-min (min-obj 'right-count (list ROW-A ROW-B)))
			(define right-max (max-obj 'right-count (list ROW-A ROW-B)))
			(- 1.0 (/ right-min right-max))
		)

		; -------------
		; Return the left-overlap similarity
		(define (compute-left-overlap-sim COL-A COL-B)
			(define left-eith (either-obj 'left-count (list COL-A COL-B)))
			(define left-both (both-obj 'left-count (list COL-A COL-B)))
			(/ left-both left-eith)
		)

		; Return the right-overlap similarity
		(define (compute-right-overlap-sim ROW-A ROW-B)
			(define right-eith (either-obj 'right-count (list ROW-A ROW-B)))
			(define right-both (both-obj 'right-count (list ROW-A ROW-B)))
			(/ right-both right-eith)
		)

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-product)    (apply compute-left-product args))
				((right-product)   (apply compute-right-product args))
				((left-cosine)     (apply compute-left-cosine args))
				((right-cosine)    (apply compute-right-cosine args))
				((left-jaccard)    (apply compute-left-jaccard-dist args))
				((right-jaccard)   (apply compute-right-jaccard-dist args))
				((left-overlap)    (apply compute-left-overlap-sim args))
				((right-overlap)   (apply compute-right-overlap-sim args))
				(else              (apply LLOBJ (cons message args))))
			)))

; ---------------------------------------------------------------------
