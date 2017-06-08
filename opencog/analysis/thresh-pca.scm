;
; thresh-pca.scm
;
; Perform a Thresholding Principal Component Analsysis
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; A simple, low-brow iterative method to get a quick approximation
; to the first PCA component.  This will be used to create a kind of
; sparse PCA vector, by passing it through a sigmoid function.
; This will in turn be used identify set membership. A detailed
; description is available in the diary.
;
; Write p(x,y) for the probability of the ordered pair, as elsewhere
; throughout this code.  Write P for the index-free matrix; write P^T
; for it's transpose.  Then, given an input vector b, there are two
; kinds of iterations one can perform: a "left" and a "right" iteration.
; The "left iteration" is defined as PP^Tb and the "right iteration" as
; P^TPb.  Note that the dimension of "b" is different for these two cases,
; with dim b = right-dim of P for the right-iteration case.
;
; In index notation, a single "left iteration" step is then the
; computation of the double sum
;
;    s(w) = sum_y p(w,y) sum_x p(x,y) b(x)
;
; and the "right iteration" is
;
;    s(z) = sum_x p(x,z) sum_y p(x,y) b(y)
;
; Because P is sparse, it makes the most sense to compute the inner sum
; "on demand", for only those index values where the outer sum is
; non-vanishing.
;
; How should the vectors 'b' and 's' be represented?  From the point of
; view of scheme, a list of pairs (num . Atom) is adequate and fast.
; The other alternative is a list of Atom, with a numeric value attached
; to some Value on that Atom. Both can be supprted with a getter.
;
; In the following a list of pairs (num . Atom) will be called an "fvec",
; and this will be the primary internal-use format.
;
; See the FAQ for why heavy-weight numerical calculations are being done
; in scheme instead of C++.
;
; ---------------------------------------------------------------------
;

(define (make-thresh-pca LLOBJ)
	(let ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ))
		)

		; --------------------
		; Return an fvec of items with uniform weighting. This is a
		; unit vector. i.e. its dot-product with itself is 1.0.
		(define (unit-fvec ITEM-LIST)
			(define weight (/ 1 (sqrt (length ITEM-LIST))))
			(map
				(lambda (item) (cons weight item))
				ITEM-LIST))

		; Apply equal weighting to all elements of the left-basis
		; This is the starting vector for one step of left-iterate.
		(define (left-init)
			(unit-vec (star-obj 'left-basis)))

		(define (right-init)
			(unit-vec (star-obj 'right-basis)))
		; --------------------
		(define 

		; --------------------

		(define (left-iter-once FVEC)
		)

		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-initial)           (left-init)
				((right-initial)          (right-init)
				((left-iterate)	        (apply left-iter-once))
				(else (apply llobj        (cons message args))))))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
