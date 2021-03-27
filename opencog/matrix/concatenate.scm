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
	(let (id-string (string-append "(" (LLA 'id) "." (LLB 'id) ")"))
			(a-stars (add-pair-stars LLA))
			(b-stars (add-pair-stars LLB))
			(l-basis '())
			(l-size 0)
			(is-from-a? #f)
			(r-type-a? #f)
		)

		; Initialize if not initialized.
		(define (init-ra)
			(if (not r-type-a?)
				(set! r-type-a?
					(make-aset-predicate (a-stars 'right-basis)))))

		; ---------------
		; Name and id of this object.
		; Caution: other objects, e.g. those that access marginals,
		; use the id as part of the marginal label. This happens
		; whenever 'filters? is #t. So don't just change the id;
		; doing so will corrupt existing databases using this code.
		(define (get-name)
			(string-append "Left concatenation " (LLA 'name) " . " (LLB 'name)))
		(define (get-id) id-string)

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

		; Return the pair, if it exists.
		; Brute force; try A, then B.
		(define (get-pair L-ATOM R-ATOM)
			(define maybe-a (LLA 'get-pair L-ATOM R-ATOM))
			(if (not (nil? maybe-a)) maybe-a
				(LLB 'get-pair L-ATOM R-ATOM)))

		; Create a pair, whether or not it exists.
		; Assumes that the right-basis of LLA is disjoint from the
		; right-basis of LLB. Thus, we can unambigously know which
		; type to create. This assumes the user is only trying to
		; create wild-cards with this function; it breaks down utterly
		; for anything else.
		(define (make-pair L-ATOM R-ATOM)
			(init-ra)
			(if (r-type-a? R-ATOM)
				(LLA 'make-pair L-ATOM R-ATOM)
				(LLB 'make-pair L-ATOM R-ATOM)))

		; Return the count on the pair, if it exists.
		(define (get-pair-count L-ATOM R-ATOM)
			(define maybe-a (LLA 'get-pair L-ATOM R-ATOM))
			(if (not (nil? maybe-a))
				(LLA 'get-count maybe-a)
				(LLB 'pair-count L-ATOM R-ATOM)))

		; Return the count on the pair, by delgating.
		; Maintains a cache of all atoms in LLA, and uses that
		; to delegate.
		(define (get-count PAIR)
			(if (not is-from-a?) ; initialize if not initialized.
				(set! is-from-a?
					(make-aset-predicate (a-stars 'get-all-elts))))
			(if (is-from-a? PAIR)
				(LLA 'get-count PAIR) (LLB 'get-count PAIR)))

		; Delegate left-wildcards to the two components
		; Unambiguous, since we can use the right-type to
		; figure out which one to delegate to. This works
		; safely because 'filters? is #t and so marginals are
		; labelled with the 'id of this object.
		(define (left-wildcard R-ATOM)
			(init-ra)
			(if (r-type-a? R-ATOM)
				(LLA 'left-wildcard R-ATOM)
				(LLB 'left-wildcard R-ATOM)))

		; ===================================================
		; Overloaded stars functions

		; Left basis must be the union of the two.
		(define (compute-left-basis)
			(define atom-set (make-atom-set))
			(for-each atom-set (a-stars 'left-basis))
			(for-each atom-set (b-stars 'left-basis))
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
			(append (a-stars 'right-basis) (b-stars 'right-basis)))

		(define (right-basis-size)
			(+ (a-stars 'right-basis-size) (b-stars 'right-basis-size)))

		; Just get all the parts, and append them.
		(define (get-all-elts)
			(append (a-stars 'get-all-elts) (b-stars 'get-all-elts)))



		; XXX Uh, is this neeed/correct?
		(define (clobber)
			(a-stars 'clobber)
			(b-stars 'clobber))

		; -------------
		; Return a pointer to each method that this class overloads.
		(define (provides meth)
			(case meth
				((left-basis)       left-basis)
				((right-basis)      right-basis)
				((left-basis-size)  left-basis-size)
				((right-basis-size) right-basis-size)
				((left-stars)       left-stars)
				((right-stars)      right-stars)
				((left-duals)       left-duals)
				((right-duals)      right-duals)
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

				((get-pair)         (apply get-pair args))
				((pair-count)       (apply get-pair-count args))
				((get-count)        (apply get-count args))
				((make-pair)        (apply make-pair args))
				((left-wildcard)    (apply get-left-wildcard args))
				; ((right-wildcard) get-right-wildcard)
				; ((wild-wild) get-wild-wild)
				((fetch-pairs)      (fetch-all-pairs))

				; Overloaded stars functions
				((left-basis)       (left-basis))
				((right-basis)      (right-basis))
				((left-basis-size)  (left-basis-size))
				((right-basis-size) (right-basis-size))
				((left-stars)       (apply left-stars args))
				((right-stars)      (apply right-stars args))
				((left-duals)       (apply left-duals args))
				((right-duals)      (apply right-duals args))
				((get-all-elts)     (get-all-elts))
				((clobber)          (clobber))

				((provides)         (apply provides args))
				((filters?)         #t)

				; Block anything that we can't handle.
				(else               (throw 'bad-use 'make-concatenation
					(format #f "Sorry, method ~A not available!" message)))
	)))
)

; ---------------------------------------------------------------------
