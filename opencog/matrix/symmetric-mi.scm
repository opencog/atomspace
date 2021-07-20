;
; symmetric-mi.scm
;
; Define API for computing the symmetric mutual information, which
; is obtained from either one of the symmetric sparse matrices MM^T
; or M^TM, given a non-symmetric sparse matrix M.
;
; Copyright (c) 2017, 2018 Linas Vepstas
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

(define*-public (add-symmetric-mi-compute LLOBJ
	#:optional (GET-CNT 'get-count))
"
  add-symmetric-mi-compute LLOBJ - Extend LLOBJ with methods to compute
  the symmetric mutual information from the symmetric matrices M^TM or
  MM^T, for M==LLOBJ a non-symmetric matrix.

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

  These two symmtric matricies can be re-interpreted as joint (pair)
  probabilities (frequencies) simply by normalizing by the total count.
  That is, if f(y,z) is a symmetric matrix, then
       p(y,z) = f(y,z) / f(*,*)
  can be interpreted as a (frequentist) joint probability of observing y
  and z together. From this, one can construct the (fractional) mutual
  information:
       FMI(y,z) = log_2 p(y,z) / p(y) p(z)
  where p(y) = p(y,*) = p(*,y) is the marginal probability. The full MI
  is just of course
       MI(y,z) = p(y,z) FMI(y,z)

  Provided methods:
  -----------------
  'mtm-mi  CA CB -- return the left-MI between columns CA and CB.
  'mtm-fmi CA CB -- return the left-FMI between columns CA and CB.
  'mmt-mi  RA RB -- return the right-MI between rows RA and RB.
  'mmt-fmi RA RB -- return the right-FMI between rows RA and RB.
  'mtm-joint-prob CA CB - return the joint probability between columns
  'mmr-joint-prob RA RB - return the joint probability between rows
  'mtm-marginal COL - return the marginal probability for column COL
  'mmt-marginal ROW - return the marginal probability for row ROW

  Arguments:
  ----------
  Here, the LLOBJ is expected to be an object defining a sparse matrix,
  with valid counts associated with each pair. It is expected to have
  all the usual sparse-matrix methods on it.

  Importantly: It is assumed that the transpose-marginals on it have
  been previously computed, and are available in the AtomSpace. The
  mmt methods on this object require that the mmt transpose marginals
  to have ben precomputed. Likewise for mtm; it is typical that most
  users will be interested in only one or the other, and thus only one
  of these need to be precomputed.

  By default, the N(x,y) is taken to be the 'get-count method on LLOBJ,
  i.e. it is literally the count. The optional argument GET-CNT allows
  this to be over-ridden with any other method that returns a number.

  If 'get-count is over-ridden, then the precomputed transpose-marginals
  should have been recomputed with the same method, else garbage will
  result.
"

	(let* ((ol2 (/ 1.0 (log 2.0)))
			(star-obj (add-pair-stars LLOBJ))
			(trans-obj (add-transpose-api star-obj))
			(prod-obj  (add-support-compute
				(add-tuple-math star-obj * GET-CNT)))

			; Cache of the totals
			(mtm-total #f)
			(mmt-total #f)
		)

		; Compute the log base two.  It can happen that log of zero
		; is requested; this is rare, but can happen when a matrix
		; has been altered so that the counts on an entire row have
		; been zeroed, and the 'set-mmt-marginals method is called
		; on that row.
		(define (log2 x y)
			(if (< 0 x) (* (log (/ x y)) ol2) (- (inf))))

		; Cache the totals, so that we can avoid fetching them,
		; over and over. They only tricky part here is that the
		; totals might not yet be available when this object is
		; defined.
		(define (set-mtm-total)
			(if (not mtm-total)
				(set! mtm-total
					(catch #t (lambda () (trans-obj 'total-mtm-count))
						(lambda (key . args) #f))))
			(if (eq? 0 mtm-total)
				(throw 'no-transpose-data 'mtm-mi
					"No transpose data available for this dataset! Did you forget to (batch-transpose)? ")))

		(define (set-mmt-total)
			(if (not mmt-total)
				(set! mmt-total
					(catch #t (lambda () (trans-obj 'total-mmt-count))
						(lambda (key . args) #f))))
			(if (eq? 0 mmt-total)
				(throw 'no-transpose-data 'mmt-mi
					"No transpose data available for this dataset! Did you forget to (batch-transpose)? ")))


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

		; Same as above, but normalized, so that the value is the
		; joint probability between COL-A and COL-B.
		(define (compute-left-prob COL-A COL-B)
			(set-mtm-total)
			(/ (compute-left-product COL-A COL-B) mtm-total))

		(define (compute-right-prob ROW-A ROW-B)
			(set-mmt-total)
			(/ (compute-right-product ROW-A ROW-B) mmt-total))

		; Marginal probabilities
		(define (compute-mtm-marginal COL)
			(set-mtm-total)
			(/ (trans-obj 'mtm-count COL) mtm-total))

		(define (compute-mmt-marginal ROW)
			(set-mmt-total)
			(/ (trans-obj 'mmt-count ROW) mmt-total))

		(define (compute-mtm-fmi COL-A COL-B)
			(define marga (trans-obj 'mtm-count COL-A))
			(define margb (trans-obj 'mtm-count COL-B))
			(define prod (compute-left-product COL-A COL-B))
			(set-mtm-total)
			(log2 (* prod mtm-total) (* marga margb)))

		(define (compute-mtm-mi COL-A COL-B)
			(define marga (trans-obj 'mtm-count COL-A))
			(define margb (trans-obj 'mtm-count COL-B))
			(define prod (compute-left-product COL-A COL-B))
			(set-mtm-total)
			(* (log2 (* prod mtm-total) (* marga margb))
				(/ prod mtm-total)))

		(define (compute-mmt-fmi ROW-A ROW-B)
			(define marga (trans-obj 'mmt-count ROW-A))
			(define margb (trans-obj 'mmt-count ROW-B))
			(define prod (compute-right-product ROW-A ROW-B))
			(set-mmt-total)
			(log2 (* prod mmt-total) (* marga margb)))

		(define (compute-mmt-mi ROW-A ROW-B)
			(define marga (trans-obj 'mmt-count ROW-A))
			(define margb (trans-obj 'mmt-count ROW-B))
			(define prod (compute-right-product ROW-A ROW-B))
			(set-mmt-total)
			(* (log2 (* prod mmt-total) (* marga margb))
				(/ prod mmt-total)))

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((mtm-mi)          (apply compute-mtm-mi args))
				((mmt-mi)          (apply compute-mmt-mi args))
				((mtm-fmi)         (apply compute-mtm-fmi args))
				((mmt-fmi)         (apply compute-mmt-fmi args))
				((mtm-joint-prob)  (apply compute-left-prob args))
				((mmt-joint-prob)  (apply compute-right-prob args))
				((mtm-marginal)    (apply compute-mtm-marginal args))
				((mmt-marginal)    (apply compute-mmt-marginal args))
				(else              (apply LLOBJ (cons message args))))
			)))

; ---------------------------------------------------------------------
