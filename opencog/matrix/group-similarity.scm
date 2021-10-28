;
; group-similarity.scm
;
; Provide similarity scores for N vectors.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Given N vectors, one may wish to determine how similar they are to
; one-another. One can, of course, compute pair-wise similarities, but
; there are N!/2 such pairs, and so this gets quickly out of hand.
; Besides, its not clear how to combine pair-wise similarities. Thus,
; it is handy to have similarity measures that work for the entire
; group, taken as a whole.  This file provides that.
;
; Currently, only one function is provided: a generalized Jaccard
; similarity. It is generalized in that it works for N vectors, and not
; just two. It is also generalized by replacing the Jaccard "min"
; function by a "democratic vote" function, where an item is accepted
; if it is shared in commmon by a majority.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog persist))

; ---------------------------------------------------------------------

(define-public (make-group-similarity LLOBJ)
"
  make-group-similarity LLOBJ - Extend LLOBJ with methods to compute
  similarities between rows or columns of the LLOBJ sparse matrix. This
  is a generalization of pair-wise similarity to the case of finding the
  mutual similarity btween three or more rows/columns in the matrix. If
  only pair-wise similarity is needed, use the `add-similarity-compute`
  object.

  Currently, this only provides a generalized Jaccard distance. This
  generalizes the conventional Jaccard distance by replacing the 'min'
  function by a 'democratic vote' function, where an item is accepted
  if it is shared in commmon by a majority.

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  Let D(x,y) be 1 if N(x,y)>0 and zero otherwise.

  Then, given a set of K columns K={a,b,...,k}, and a single row x,
  let row-supp(x, {a,b,...,k}) = D(x,a) + D(x,b) + ... + D(x,k)
  let row-cnt(x, {a,b,...,k}) = N(x,a) + N(x,b) + ... + N(x,k)
  That is, the row-supp is the total number of columns of the column-set
  that are not zero.  The row-cnt is the sum of all of the counts in
  the column set.

  Given a threshold T, the mutual support of a group of K={a,b,...,k}
  columns is given by

  mutual-row-supp(T,K) = sum_x [T < row-supp(x,K)]

  The mutual overlap (aka the mutual unweighted Jaccard similarity)
  is then
  mutual-row-overlap(T,K) = mutual-row-supp(T,K) / mutual-row-supp(0,K)

  Likewise,
  mutual-row-count(T,K) = sum_x [T < row-cnt(x,K)]

  Note that for K={a,b} just two columns that the conventional overlap
  is given by setting T=1.

  Exchanging rows and columns gives similar definitions.

  A 'noisy' variant is provided, by altering the defintion of a non-zero
  count. Let N(x,y) be the observed count for the pair (x,y), just as
  before.  Let D(x,y) be 1 if N(x,y)>noise-threshold and zero otherwise.
  This is useful for working with matrices that have low-level junk in
  the matrix entries. This junk can, of course, be eliminated by global
  trimming of the matrix (see the `add-trimmer` and related objects),
  but sometimes, it is useful to selectively ignore differences. Thus,
  the noisy variant.

  Provided methods:
  -----------------
  'row-supp returns the number as defined above. Likewise 'column-supp

  'mutual-row-supp THRESH COL-LIST returns a list of two numbers:
     mutual-row-supp(T,K) and mutual-row-supp(0,K) for T=THRESH and
     K=COL-LIST.  This is because the algo obtains the second 'for
     free' while computing the first. It's up to you to divide these,
     if you wish.

  'mutual-col-supp Likewise.

  'noise-row-supp THRESH NOISE COL-LIST same as above, but using NOISE
     to determine the no-count threshold.

  'noise-col-supp Likewise.
"
	(define (mutual-vote THRESH IDX-LIST ACCEPT-FUNC CNT-FUNC DUALS-FUNC)

		; Put all of the co-indexes on all of the indexes into a bag.
		; Add them only if the counts are above the noise-floor.
		(define insert-into-set-of-all-co-idx (make-atom-set))
		(for-each
			(lambda (IDX)
				(for-each
					(lambda (CO-IN)
						(if (ACCEPT-FUNC IDX CO-IN)
							(insert-into-set-of-all-co-idx CO-IN)))
					(DUALS-FUNC IDX)))
			IDX-LIST)

		(define list-of-all-co-idx (insert-into-set-of-all-co-idx #f))

		; Return #t if the CO-IN is shared by the majority of the
		; indexes. That is, it return #t if the sum over indexes
		; that have CO-IN is greater than THRESH.
		; If IDX are rows, then CO-IN are columns, and vice-versa.
		(define (vote-to-accept? CO-IN)
			(< THRESH
				(fold
					(lambda (IDX CNT) (+ CNT (CNT-FUNC IDX CO-IN)))
					0
					IDX-LIST)))

		; Count the particular CO-IN, if it is shared by the majority.
		(define shared-count
			(fold
				(lambda (CO-IN CNT)
					(if (vote-to-accept? CO-IN) (+ 1 CNT) CNT))
				0
				list-of-all-co-idx))

		; Return two numbers: the shared count and the total count.
		(list shared-count (length list-of-all-co-idx))
	)

	(define (noise-col-supp THRESH NOISE ROW-LIST)
		(define (acptfunc ROW COL)
			(< NOISE (LLOBJ 'pair-count ROW COL)))

		(define (cntfunc ROW COL)
			(if (acptfunc ROW COL) 1 0))

		(define (duals-func ROW) (LLOBJ 'right-duals ROW))

		; Call the common framework
		(mutual-vote THRESH ROW-LIST acptfunc cntfunc duals-func)
	)
	; Return mutual-col-supp, as defined above.
	(define (mutual-col-supp THRESH ROW-LIST)
		(noise-col-supp THRESH 0 ROW-LIST))

	(define (noise-row-supp THRESH NOISE COL-LIST)
		(define (acptfunc COL ROW)
			(< NOISE (LLOBJ 'pair-count ROW COL)))

		(define (cntfunc COL ROW)
			(if (acptfunc ROW COL) 1 0))

		(define (duals-func COL) (LLOBJ 'left-duals COL))
		(mutual-vote THRESH COL-LIST acptfunc cntfunc duals-func)
	)
	(define (mutual-row-supp THRESH COL-LIST)
		(noise-row-supp THRESH 0 COL-LIST))

	(define (mutual-col-cnt THRESH ROW-LIST)
		(define (cntfunc ROW COL) (LLOBJ 'pair-count ROW COL))
		(define (acptfunc ROW COL) (< 0 (cntfunc ROW COL)))

		(define (duals-func ROW) (LLOBJ 'right-duals ROW))
		(mutual-vote THRESH ROW-LIST acptfunc cntfunc duals-func)
	)

	(define (mutual-row-cnt THRESH COL-LIST)
		(define (cntfunc COL ROW) (LLOBJ 'pair-count ROW COL))
		(define (acptfunc COL ROW) (< 0 (cntfunc COL ROW)))

		(define (duals-func COL) (LLOBJ 'left-duals COL))
		(mutual-vote THRESH COL-LIST acptfunc cntfunc duals-func)
	)

	; -------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((mutual-col-supp) (apply mutual-col-supp args))
			((mutual-row-supp) (apply mutual-row-supp args))
			((mutual-col-cnt)  (apply mutual-col-cnt args))
			((mutual-row-cnt)  (apply mutual-row-cnt args))
			((noise-col-supp)  (apply noise-col-supp args))
			((noise-row-supp)  (apply noise-row-supp args))
			(else              (apply LLOBJ (cons message args)))))
)

; ---------------------------------------------------------------
; Example usage  (none)
