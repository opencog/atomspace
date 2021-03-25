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
