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

  Provided methods:
  -----------------
  'row-supp returns the number as defined above. Likewise 'column-supp

  'mutual-row-supp returns a list of two numbers: mutual-row-supp(T,K)
  and mutual-row-supp(0,K). This is because the algo obtains the second
  'for free' while computing the first. It's up to you to divide these,
  if you wish.
"
	(define (mutual-vote THRESH IDX-LIST CNT-FUNC BASIS-FUNC)

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

		; Put all of the co-indexes on all of the indexes int a bag.
		(define set-of-all-co-idx (make-atom-set))
		(for-each
			(lambda (IDX)
				(for-each
					(lambda (CO-IN) (set-of-all-co-idx CO-IN))
					(BASIS-FUNC IDX)))
			IDX-LIST)

		(define list-of-all-co-idx (set-of-all-co-idx #f))

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

	; Return mutual-col-supp, as dfined above.
	(define (mutual-col-supp THRESH ROW-LIST)
		(define (cntfunc ROW COL)
			(if (< 0 (LLOBJ 'pair-count ROW COL)) 1 0))

		(define (basis-func ROW) (LLOBJ 'right-basis ROW))

		; Call the common framework
		(mutual-vote THRESH ROW-LIST cntfunc basis-func)
	)

	(define (mutual-row-supp THRESH COL-LIST)
		(define (cntfunc COL ROW)
			(if (< 0 (LLOBJ 'pair-count ROW COL)) 1 0))

		(define (basis-func COL) (LLOBJ 'left-basis COL))
		(mutual-vote THRESH COL-LIST cntfunc basis-func)
	)

	(define (mutual-col-cnt THRESH ROW-LIST)
		(define (cntfunc ROW COL) (LLOBJ 'pair-count ROW COL))
		(define (basis-func ROW) (LLOBJ 'right-basis ROW))
		(mutual-vote THRESH ROW-LIST cntfunc basis-func)
	)

	(define (mutual-row-cnt THRESH COL-LIST)
		(define (cntfunc COL ROW) (LLOBJ 'pair-count ROW COL))
		(define (basis-func COL) (LLOBJ 'left-basis COL))
		(mutual-vote THRESH COL-LIST cntfunc basis-func)
	)

	; -------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((mutual-col-supp) (apply mutual-col-supp args))
			((mutual-row-supp) (apply mutual-row-supp args))
			((mutual-col-cnt)  (apply mutual-col-cnt args))
			((mutual-row-cnt)  (apply mutual-row-cnt args))
			(else              (apply LLOBJ (cons message args)))))
)

; ---------------------------------------------------------------
; Example usage  (none)
