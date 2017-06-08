;
; power-iter.scm
;
; Perform feed-forward neural-net + PCA analysis.
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
; Multiple matrix times vector.
;
; Compute    sum_y MAT(ITEM, y) VEC(y)
;
; where the set of y to be summed over is ITEM-LIST,
; and GET-VEC-VAL is a function that returns a numeric VEC(y)
; and GET-MAT-VAL is a function that returns a numeric MAT(ITEM, y)
;
(define (multiply-mat-vec ITEM GET-MAT-VAL ITEM-LIST GET-VEC-VAL)
	(fold
		(lambda (sum, item)
			(+ sum (* (GET-MAT-VAL ITEM item) (GET-VEC-VAL item))))
		0.0
		ITEM-LIST)
)

; ---------------------------------------------------------------------
;
; Return an fvec of items with uniform weighting. This is a unit vector.
; i.e. its dot-product with itself is 1.0.
;
(define (unit-fvec ITEM-LIST)
	(define weight (sqrt (length ITEM-LIST)))
	(map
		(lambda (item) (cons weight item))
		ITEM-LIST)
)

; ---------------------------------------------------------------------
