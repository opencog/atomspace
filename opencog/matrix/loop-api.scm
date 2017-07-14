;
; loop-api.scm
;
; Loop over all pairs of items.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The matrix object API below provides several different looping
; constructs, to vist every non-zero entry in a sparse matrix.
; The matrix is accessed using the wild-card stars object.
;
; In the general case, access to this structure is provided by methods
; on the "low-level API". These include:
;   'item-pair, which should return high-level pair, given the
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
; Extend the LLOBJ with additional methods to compute observation
; frequencies and entropies for pairs, including partial-sum entropies
; (mutual information) for the left and right side of each pair.
;
; The LLOBJ must have valid left and right wild-card counts on it.
; These need to have been previously computed, before methods on
; this class are called.
;
(define (loop-api LLOBJ)

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
; ---------------------------------------------------------------------
