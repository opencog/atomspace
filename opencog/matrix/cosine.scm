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

(define*-public (add-similarity-compute LLOBJ
	#:optional (GET-CNT 'get-count))
"
  add-similarity-compute LLOBJ - Extend LLOBJ with methods to compute
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

  The left-product is the same thing as an entry in the symmetric
  matrix M^TM (with M==LLOBJ and ^T the transpose). That is,
       left-product(y,z) = [M^TM](y,z)
  Likewise, the right-product is an entry in the matrix MM^T.

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
  The Jaccard distance can be defined as one minus the Jaccard
  similarity, which is defined as

      left-jacc-sim(a,b) = sum_x min (N(x,a), N(x,b)) /
               sum_x max (N(x,a), N(x,b))

  The above is also sometimes called the 'Ruzicka distances'.

  Conditional Jaccard:
  --------------------
  The Jaccard distance gives an awkward result if the two count vectors
  are colinear but of different lengths. For most problems considered
  here, colinear count vectors should be considered to be equal. Thus,
  a normalized version is defined below. The normalization is to use
  the conditional probability instead of the count. (Normalization by
  Euclidean lenght does noe make sense for counts).

  The conditional probability is then defined as
       p(x|a) = N(x,a) / N(*,a)

  The conditional similarity is defined as

      left-cond-jacc-sim(a,b) = sum_x min (p(x|a), p(x|b)) /
               sum_x max (p(x|a), p(x|b))

  Probability Jaccard:
  --------------------
  The Proability-Jaccard distance can be defined as one minus the
  Probability-Jaccard similarity. The later is defined as

      left-prjacc-sim(a,b) = sum_x
             [sum_y max {N(y,a)/N(x,a), N(y,b)/N(x,b) }]^-1
         = sum_x
             [sum_y max {p(y|a)/p(x|a), p(y|b)/p(x|b) }]^-1

  Note the x sum is a sum over reciprocals of a sum.  This form is
  discussed in greater detail in Wikipedia; it has the advantage of
  providing 'maximally consistent sampling'. Its appropriate for the
  case where the counts really are meant to be interpreted as providing
  probabilistic frequencies.


  Overlap:
  --------
  The overlap similarity simply counts how many common non-zero entries
  are shared in common between two rows or columns.  That is, it is

      left-overlap(y,z) = sum_x (0 < N(x,y)) * (0 < N(x,z)) /
               sum_x (0 < N(x,y) + N(x,z))

  If is effectively the unweighted Jaccard similarity, where each point
  is given exactly the same weight (zero or non-zero).

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
		; The prod-obj takes the product of pairs of matrix entries,
		; and the 'left-count method just adds them up.  Equivalently,
		; we could just sum over the left-stars ourselves, but this
		; would take three lines of code instead of one.
		(define (compute-left-product COL-A COL-B)
			(prod-obj 'left-count (list COL-A COL-B)))

		; Return the vector product of row A and row B
		(define (compute-right-product ROW-A ROW-B)
			(prod-obj 'right-count (list ROW-A ROW-B)))

		; -------------
		(define (get-left-length COL) (supp-obj 'left-length COL))
		(define (get-right-length ROW) (supp-obj 'right-length ROW))

		; Return the cosine of the angle between column A and B.
		; The cosine as defined above (the usual textbook definition).
		(define (compute-left-cosine COL-A COL-B)
			(define prod (compute-left-product COL-A COL-B))
			; If the length is exactly zero, then (eqv? 0.0 0) fails!!
			(define deno (exact->inexact (*
				(get-left-length COL-A)
				(get-left-length COL-B))))
			(if (eqv? 0.0 deno) 0.0 (/ prod deno)))

		; As above, but for the rows.
		(define (compute-right-cosine ROW-A ROW-B)
			(define prod (compute-right-product ROW-A ROW-B))
			(define deno (exact->inexact (*
				(get-right-length ROW-A)
				(get-right-length ROW-B))))
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
		; Return the conditional jaccard distance
		; IDX is a row or a column
		; METH is either 'left-count or 'right-count
		; 'left-count is N(*,COL) and 'right-count is N(ROW,*)
		(define (compute-cond-jacc-dist IDX-A IDX-B METH)
			(define osum-A (/ 1.0 (supp-obj METH IDX-A)))
			(define osum-B (/ 1.0 (supp-obj METH IDX-B)))
			(define (pmin x y)
				(min (* x osum-A) (* y osum-B)))
			(define (pmax x y)
				(max (* x osum-A) (* y osum-B)))
			(define pmin-obj (add-support-compute
				(add-tuple-math star-obj pmin GET-CNT)))
			(define pmax-obj (add-support-compute
				(add-tuple-math star-obj pmax GET-CNT)))
			(define j-min (pmin-obj METH (list IDX-A IDX-B)))
			(define j-max (pmax-obj METH (list IDX-A IDX-B)))
			(- 1.0 (/ j-min j-max))
		)

		(define (compute-left-cond-jacc-dist COL-A COL-B)
			(compute-cond-jacc-dist COL-A COL-B 'left-count))

		(define (compute-right-cond-jacc-dist ROW-A ROW-B)
			(compute-cond-jacc-dist ROW-A ROW-B 'right-count))

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
		; Return the probability-jaccard distance
		; IDX is a row or a column
		; METH is either 'left-count or 'right-count
		; 'left-count is N(*,COL) and 'right-count is N(ROW,*)
		(define (compute-prob-jaccard-dist IDX-A IDX-B METH)

			; Given weights `weig-a` and `weig-b` take the sum of the
			; maxes of the two rows, weighted by the weights.
			(define (weighted-max weig-a weig-b)
				(define (wmax a b)
					(max (* a weig-a) (* b weig-b)))
				(define wmax-obj
					(add-support-compute
						(add-tuple-math star-obj wmax GET-CNT)))
				(wmax-obj METH (list IDX-A IDX-B)))

			; Given two numbers, compute the denominator of the
			; prob-jaccard sum. The tuple math object iterates
			; over the union, so we have to manually reject counts
			; where one is zero (i.e. to get the intersection)
			(define (denom na nb)
				(if (and (< 0.0 na) (< 0.0 nb))
					(/ 1.0 (weighted-max (/ 1.0 na) (/ 1.0 nb)))
					0.0))

			; Use the tuple math object to run over both rows
			(define pjac-obj
				(add-support-compute
					(add-tuple-math star-obj denom GET-CNT)))

			(- 1.0 (pjac-obj METH (list IDX-A IDX-B)))
		)

		; Return the left-probability-jaccard distance
		(define (compute-left-prob-jaccard-dist COL-A COL-B)
			(compute-prob-jaccard-dist COL-A COL-B 'left-count))

		; Return the right-probability-jaccard distance
		(define (compute-right-prob-jaccard-dist COL-A COL-B)
			(compute-prob-jaccard-dist COL-A COL-B 'right-count))

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
				((left-prjaccard)  (apply compute-left-prob-jaccard-dist args))
				((right-prjaccard) (apply compute-right-prob-jaccard-dist args))
				((left-cond-jacc)  (apply compute-left-cond-jacc-dist args))
				((right-cond-jacc) (apply compute-right-cond-jacc-dist args))
				((left-overlap)    (apply compute-left-overlap-sim args))
				((right-overlap)   (apply compute-right-overlap-sim args))
				(else              (apply LLOBJ (cons message args))))
			)))

; ---------------------------------------------------------------------
