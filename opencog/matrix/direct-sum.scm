;
; direct-sum.scm
;
; Define an API for taking the direct sum of two matrices.
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
; the two matrices were stacked side-by-side.
;
; A protoypical example of the need for this the combination of a
; word-disjunct matrix with a shape matrix. A word-disjunct matrix has
; the form of `(Section (Word "left-item") (Disjunct ...))` where the
; Disjunct has Words inside of it. A shape matrix is similar, but this
; time isolating the Words in the Disjunct, and making them be the left
; item. Both matrices have the same type of left-item. Thus,
; concatenating them allows one left item to index a union of the
; right-items of the two matrices. Each row of this matrix is a vector,
; the concatenation of the corresponding rows in each matrix.
;
; More formally, this creates the direct sum of these two matrices,
; after taking the set-union of the left and the right indexes on
; the matrix.  This avoids any assumptions that the types of the
; indexes are the same, or are even compatible; it simply mashes them
; up to create a union-type.
;
; In index notation, if matrix D is the direct sum of matrix A and B,
; that is, if D = A ⊕ B, then the left basis of D is the set-union of
; the left basis of A and of B, and likewise the right-basis. The
; matrix elements D_ij are either A_ij or B_ij, depending on which one
; exists. It is generally assumed that either one or the other exists,
; but not both. It is ill-defined if both exist.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define-public (make-left-concatenation LLA LLB)
"
  make-left-concatenation LLA LLB -- concatenate/append/sum A and B

  Given objects LLA and LLB
"
	(let ((id-string (string-append "(" (LLA 'id) "+" (LLB 'id) ")"))
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
			(string-append "Left concatenation of \"" (LLA 'name)
				 "\" and \"" (LLB 'name) "\""))

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
			(if (eq? 0 l-size) (set! l-size (length (left-basis))))
			l-size)

xxxxxxxxx
		; The right basis is easy: since the items are completely
		; different, all we have to do is to append them.
		(define (right-basis)
			(append (a-stars 'right-basis) (b-stars 'right-basis)))

		(define (right-basis-size)
			(+ (a-stars 'right-basis-size) (b-stars 'right-basis-size)))

		; Delegate left stars and duals according to the type
		; of the right-atom.
		(define (left-stars R-ATOM)
			(init-ra)
			(if (r-type-a? R-ATOM)
				(a-stars 'left-stars R-ATOM)
				(b-stars 'left-stars R-ATOM)))

		(define (left-duals R-ATOM)
			(init-ra)
			(if (r-type-a? R-ATOM)
				(a-stars 'left-duals R-ATOM)
				(b-stars 'left-duals R-ATOM)))

		; Right stars and duals are a simple concatenaition
		(define (right-stars L-ATOM)
			(append
				(a-stars 'right-stars L-ATOM)
				(b-stars 'right-stars L-ATOM)))

		(define (right-duals L-ATOM)
			(append
				(a-stars 'right-duals L-ATOM)
				(b-stars 'right-duals L-ATOM)))

		; Just get all the parts, and append them.
		(define (get-all-elts)
			(append (a-stars 'get-all-elts) (b-stars 'get-all-elts)))

		; XXX Uh, is this needed/correct?
		(define (clobber)
			(a-stars 'clobber)
			(b-stars 'clobber))

		; -------------
		(define (help)
			(format #t
				(string-append
"This is the `left-concatentation` of \"~A\" and \"~A\"\n"
"It allows these two objects to share a common left-basis\n"
"while concatenating the right-basis. For more information,\n"
"say `,d left-concatentation` or `,describe left-concatentation`\n"
"at the guile prompt, or just use the 'describe method on this\n"
"object.\n"
)
				(LLA 'id) (LLB 'id)))

		(define (describe)
			(display (procedure-property make-left-concatenation 'documentation)))

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
				((left-wildcard)    (apply left-wildcard args))
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

				((help)             (help))
				((describe)         (describe))

				; Block anything that we can't handle.
				(else               (throw 'bad-use 'make-concatenation
					(format #f "Sorry, method ~A not available!" message)))
	)))
)

; ---------------------------------------------------------------------
