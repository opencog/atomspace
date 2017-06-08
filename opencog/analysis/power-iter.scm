;
; power-iter.scm
;
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
