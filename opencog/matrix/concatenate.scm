;
; concatenate.scm
;
; Define API's for concatenating two matrices. These can be concatenated
; left-right, or above-below.
;
; Copyright (c) 2021 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Two matrices may share the same rows (row types), but have completely
; different columns (column types); it can be useful to work with a
; matrix that is the concatenation of these two, so that the combined
; matrix has rows that have entries from one, and from the other, as if
; the two matrices were stacked side-by-side. Alternately, if the have
; the same column labels, but different rows, they can be stacked one
; above the other.
;
; In diagrams, if [A] and [B] are two matrices, then thier left-
; concatenation is [L] = [AB] and the right-concatenation is 
; [R] = [A]
;       [B]
;
; In index notation, if matrix A has n columns, then L_ij = A_ij if j<n
; and L_ij = B_i(j-n) if j>n. Likewise for R, transposing rows and
; columns.
;
; The code here takes two matrix-API objects, and creates a single
; matrix API. Note that there can be some difficulties with the API,
; as the row or column types might not be consistenst across the whole
; matrix.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define-public (make-concatenation LLA LLB)
"
  left-concatenation
"

	(let ((l-basis '())
			(l-size 0)
		)

		; ---------------
		; Right type can be either one of two things...
		; This is a provisional hack, for now. Not sure if it makes sense.
		(define (get-right-type)
			(ChoiceLink (LLA 'right-type) (LLB 'right-type)))
		(define (get-pair-type)
			(ChoiceLink (LLA 'pair-type) (LLB 'pair-type)))

		; ---------------
		; Delegate the pair fetching to each subobject.
		(define (fetch-all-pairs)
			(LLA 'fetch-pairs)
			(LLB 'fetch-pairs))

		; ---------------
		(define (get-name)
			(string-append (LLA 'name) " . " (LLB 'name)))
		(define (get-id)
			(string-append (LLA 'id) "." (LLB 'id)))

		; ===================================================
		; Overloaded stars functions

		; Left basis must be the union of the two.
		(define (compute-left-basis)
			(define atom-set (make-atom-set))
			(for-each atom-set (LLA 'left-basis))
			(for-each atom-set (LLB 'left-basis))
			(atom-set #f)
		)

		(define (left-basis)
			(if (null? l-basis) (set! l-basis (compute-left-basis)))
			l-basis)

		(define (left-basis-size)
			(if (eq? 0 l-size) (set! l-size (length (get-left-basis))))
			l-size)

		; The right basis is easy: since the items are completely
		; different, all we have to do is to append them.
		(define (right-basis)
			(append (LLA 'right-basis) (LLB 'right-basis)))

		(define (right-basis-size)
			(+ (LLA 'right-basis-size) (LLB 'right-basis-size)))

		; Just get all the parts, and append them.
		(define (get-all-elts)
			(append (LLA 'get-all-elts) (LLB 'get-all-elts)))

		; XXX Uh, is this neeed/correct?
		(define (clobber)
			(LLA 'clobber)
			(LLB 'clobber))

		; -------------
		; Return a pointer to each method that this class overloads.
		(define (provides meth)
			(case meth
				((left-basis)       left-basis)
				((right-basis)      right-basis)
				((left-basis-size)  left-basis-size)
				((right-basis-size) right-basis-size)
				((get-all-elts)     get-all-elts)
				((clobber)          clobber)
			))

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((name)             (get-name))
				((id)               (get-id))
				((left-type)        (apply LLA message))
				((right-type)       (get-right-type))
				((pair-type)        (get-pair-type))

				; ((pair-count) get-pair-count)
				; ((get-pair) get-pair)
				; ((get-count) get-count)
				; ((make-pair) make-pair)
				; ((left-wildcard) get-left-wildcard)
				; ((right-wildcard) get-right-wildcard)
				; ((wild-wild) get-wild-wild)
				((fetch-pairs)      (fetch-all-pairs))

				; Overloaded stars functions
				((left-basis)       (left-basis))
				((right-basis)      (right-basis))
				((left-basis-size)  (left-basis-size))
				((right-basis-size) (right-basis-size))
				((get-all-elts)     (get-all-elts))
				((clobber)          (clobber))

				((provides)         (apply provides args))
				((filters?)         #f)

				; Block anything that we can't handle.
				(else               (throw 'bad-use 'make-concatenation
					(format #f "Sorry, method ~A not available!" message)))
	)))
)

; ---------------------------------------------------------------------
