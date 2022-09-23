;
; ortho-ensemble.scm
;
; Define API for recasting symmetric matrices as Gaussian Orthogonal
; Ensembles. Given a (non-sparse) symmetric matix M, renormalize it so
; that the rows/columns of the matrix can be taken to be vectors on the
; unit sphere S_{N-1} (when the matrix has dimension N x N).
;
; Copyright (c) 2022 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Search the internet for infor on Gaussian Orthogonal Ensembles and
; Spin Glasses.
;
; The code here does nothing fancy or sophisticated; it just
; renormalizes the columns and rows.
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define*-public (add-gaussian-ortho-api LLOBJ
	#:optional (GET-CNT 'get-count)
	#:key (ID (LLOBJ 'id)))
"
  add-gaussian-ortho-compute LLOBJ - Extend LLOBJ with methods to
  normalize a symmetric matrix such that the mean of the matrix
  entries is zero, and the standard deviation is one. After such a
  normalization, the rows and columns can be taken to be vectors
  (approximately) uniformly distributed about the origin, and when
  normalized to unit length, to be (approximately) uniformly
  distributed on the unit sphere.
"

	(define star-obj (add-pair-stars LLOBJ))
	(define (get-cnt x) (LLOBJ GET-CNT x))
	(define mean 0)
	(define sdev 1)

	; Note that get-cnt returns exact zero when a matrix element is
	; missing. Else it might return floating zero or even negative
	; numbers, and we do wnat to handle those. Matrices with MI in
	; them use -inf.0 to denote absence.
	(define (valid? VAL) (and (not (eqv? 0 VAL)) (< -inf.0 VAL)))

	; Return a list holding the mean and RMS for the matrix.
	(define (compute-mean-rms)
		(define ntot 0)
		(define sumc 0)
		(define susq 0)
		(for-each
			(lambda (ITM)
				(define cnt (get-cnt ITM))
				(when (valid? cnt)
					(set! ntot (+ ntot 1))
					(set! sumc (+ sumc cnt))
					(set! susq (+ susq (* cnt cnt)))))
			(star-obj 'get-all-elts))
		(define avg (/ sumc ntot))
		(define msq (/ susq ntot))
		(define rms (sqrt (- msq (* avg avg))))
		(list avg rms)
	)

	; Get the mean and rms
	(define (get-mean-rms)
		(if (eqv? mean 0)
			(let ((ar (compute-mean-rms)))
				(set! mean (first ar))
				(set! sdev (second ar))))
		(list mean sdev))

	; -------------

	; Get the renormalized count. Just a simple rescaling.
	; Invalid matrix entries are mapped to exact zero.
	(define (get-renorm-count PR)
		(if (eqv? mean 0) (get-mean-rms)) ; just in case.
		(define raw (get-cnt PR))
		(if (valid? raw) (/ (- raw mean) sdev) 0))

	; -------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((mean-rms)         (get-mean-rms))
			((get-count)        (apply get-renorm-count args))
			(else               (apply LLOBJ (cons message args))))
	))

; ---------------------------------------------------------------------
