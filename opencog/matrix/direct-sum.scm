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
; More formally still, calling this thing a "direct sum" is an abuse of
; the formal definition of a direct sum. In many ways, it resembles just
; an "ordinary sum" of two matrices. However, because this is enlarging
; the set of basis elements, it is not "just" the sum of two matrices.
; It is creating a new union type from the types of the two matrices;
; an ordinary sum normally requires that the types of the indexes be
; the same. There's no obvious formal term in mathematics that I know
; of, that would apply here. "Direct sum" seems close enough.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define-public (direct-sum LLA LLB)
"
  direct-sum LLA LLB -- provide a matrix API for the 'sum' of A and B.

  This has aspects of being a concatenation of A and B, of being an
  'ordinary' sum of A and B, and of being a direct sum, .. kind-of.

  Given objects LLA and LLB, this presents a new object whose
  left-basis is the set-union of the left-basis of A and B, and
  likewise the right basis. The matrix elements are likewise the
  set-union; it is assumed that the matrix elements of A and B are
  *disjoint sets*, so that the union is well-defined. If they are
  not disjoint, then the behavior is ill-defined (i.e. it is not
  currently specified.)

  The current implementation also assumes that either the left-basis
  of A and B are disjoint, or that the right-basis is. The current
  design of this whole module makes it kind-of hard if not impossible
  for this not to be the case. So this is a reasonable assumption.
"
	(let* ((id-string (string-append "(" (LLA 'id) "⊕" (LLB 'id) ")"))
			(a-stars (add-pair-stars LLA))
			(b-stars (add-pair-stars LLB))
			(l-basis '())
			(r-basis '())
			(l-size 0)
			(r-size 0)
			(is-from-a? #f)
			(type-a? #f)
			(in-base? #f)
			(disjoint-left #f)
			(disjoint-right #f)
			(distinct-type #f)
			(pred-node (PredicateNode
				(string-append "*-Direct Sum Wild " id-string)))
			(left-wnode (AnyNode "left-wild-direct-sum"))
			(right-wnode (AnyNode "right-wild-direct-sum"))
		)

		; Initialize predicates for the left-right basis members.
		(define (init-a-base)
			(when (not type-a?)

				; If the right-types are different, then clearly they're
				; disjoint.
				(set! disjoint-left
					(not (equal? (LLA 'left-type) (LLB 'left-type))))
				(set! disjoint-right
					(not (equal? (LLA 'right-type) (LLB 'right-type))))

				(set! distinct-type (or disjoint-left disjoint-right))

				; Hmm. Interesting. A direct sum of the same kind.
				(when (not distinct-type)

					; The bases are disjoint, if the number of elts in
					; the union is equal to the num elts in each part.
					(set! disjoint-left
						 (= (+ (LLA 'left-basis-size) (LLB 'left-basis-size))
								(left-basis-size)))
					(set! disjoint-right
						 (= (+ (LLA 'right-basis-size) (LLB 'right-basis-size))
								(right-basis-size)))

					; Either the left or the right basis must be disjoint.
					(if (not (or disjoint-left disjoint-right))
						(throw 'wrong-type-arg 'direct-sum
							"Either the left or the right basis must be disjoint!"))
				)

				; Actually initialize the predicates.
				(set! in-base?
					(if disjoint-left
						(make-aset-predicate (a-stars 'left-basis))
						(make-aset-predicate (a-stars 'right-basis))))

				; Return #t if one of the two atoms belongs to the
				; disjoint basis of LLA
				(set! type-a?
					(lambda (L-ATOM R-ATOM)
						(or
							(and disjoint-left (in-base? L-ATOM))
							(and disjoint-right (in-base? R-ATOM)))))
			))

		; Initialize predicate for the members of A
		(define (init-a-set)
			(if (not is-from-a?) ; initialize if not initialized.
				(set! is-from-a?
					(make-aset-predicate (a-stars 'get-all-elts)))))

		; ---------------
		; Name and id of this object.
		(define (get-name)
			(string-append "Direct sum of '" (LLA 'name)
				 "' and '" (LLB 'name) "'"))

		; Caution: other objects, e.g. those that access marginals,
		; use the id as part of the marginal label. This happens
		; whenever 'filters? is #t. So don't just change the id;
		; doing so will corrupt existing databases using this code.
		(define (get-id) id-string)

		; ---------------
		; Types ...
		; This is a provisional hack, for now. Not sure if it makes sense.
		(define (get-left-type)
			(define at (LLA 'left-type))
			(define bt (LLB 'left-type))
			(define tat (if (cog-atom? at) at (Type at)))
			(define tbt (if (cog-atom? bt) bt (Type bt)))
			(if (equal? tat tbt) tat (TypeChoice tat tbt)))

		(define (get-right-type)
			(define at (LLA 'right-type))
			(define bt (LLB 'right-type))
			(define tat (if (cog-atom? at) at (Type at)))
			(define tbt (if (cog-atom? bt) bt (Type bt)))
			(if (equal? tat tbt) tat (TypeChoice tat tbt)))

		(define (get-pair-type)
			(define at (LLA 'pair-type))
			(define bt (LLB 'pair-type))
			(define tat (if (cog-atom? at) at (Type at)))
			(define tbt (if (cog-atom? bt) bt (Type bt)))
			(if (equal? tat tbt) tat (TypeChoice tat tbt)))

		; ---------------
		; Delegate the pair fetching to each subobject.
		; We must fetch any wild-cards that we created!
		(define (fetch-all-pairs)
			(LLA 'fetch-pairs)
			(LLB 'fetch-pairs)
			(fetch-incoming-by-type pred-node 'EvaluationLink)
			*unspecified*
		)

		; ---------------

		; Return the pair, if it exists.
		; Brute force; try A, then B.
		(define (get-pair L-ATOM R-ATOM)
			(define maybe-a (LLA 'get-pair L-ATOM R-ATOM))
			(if (not (nil? maybe-a)) maybe-a
				(LLB 'get-pair L-ATOM R-ATOM)))

		; Is the type of ATM equal to the type MTH?
		; MTH can be either a symbol e.g. 'Concept or a Type atom.
		; XXX possible bug?? what is MTH is a TypeChoiceLink?
		; Then maybe we should scane the type choices?
		; There's some atomspace utility that handles this already...
		(define (symb-comp? ATM MTH)
			(if (cog-atom? MTH)
				(equal? (Type (cog-type ATM)) MTH)
				(equal? (cog-type ATM) MTH)))

		; Create a pair, whether or not it exists.
		; Assumes that either the left or right types are distinct
		; (and so we can dispatch based on the types) or, if not,
		; then that the atoms are in left/right basis, (and can thus
		; be dispatched based on membership.) If this is not the case,
		; then this attempts to handle the special case of one or the
		; other being a VariableNode, and tries to do the right thing.
		; If this is not the case, it will return undefined results.
		; (which may be crazy.)
		(define (make-ordinary-pair L-ATOM R-ATOM)
			(if distinct-type
				(if disjoint-left
					(if (symb-comp? L-ATOM (LLA 'left-type))
						(LLA 'make-pair L-ATOM R-ATOM)
						(LLB 'make-pair L-ATOM R-ATOM))
					(if (symb-comp? R-ATOM (LLA 'right-type))
						(LLA 'make-pair L-ATOM R-ATOM)
						(LLB 'make-pair L-ATOM R-ATOM)))
				(if (type-a? L-ATOM R-ATOM)
					(LLA 'make-pair L-ATOM R-ATOM)
					(LLB 'make-pair L-ATOM R-ATOM))))

		(define (make-left-variable-pair L-ATOM R-ATOM)
			(if disjoint-right
				(if (symb-comp? R-ATOM (LLA 'right-type))
					(LLA 'make-pair L-ATOM R-ATOM)
					(LLB 'make-pair L-ATOM R-ATOM))
				(if (equal? (cog-type (get-pair-type)) 'TypeChoice)
					(ChoiceLink
						(LLA 'make-pair L-ATOM R-ATOM)
						(LLB 'make-pair L-ATOM R-ATOM))
					(throw 'invalid 'direct-sum "Don't know how to make this pair"))))

		(define (make-right-variable-pair L-ATOM R-ATOM)
			(if disjoint-left
				(if (symb-comp? L-ATOM (LLA 'left-type))
					(LLA 'make-pair L-ATOM R-ATOM)
					(LLB 'make-pair L-ATOM R-ATOM))
				(if (equal? (cog-type (get-pair-type)) 'TypeChoice)
					(ChoiceLink
						(LLA 'make-pair L-ATOM R-ATOM)
						(LLB 'make-pair L-ATOM R-ATOM))
					(throw 'invalid 'direct-sum "Don't know how to make this pair"))))

		(define (make-pair L-ATOM R-ATOM)
			(init-a-base)
			(cond
				((equal? (cog-type L-ATOM) 'VariableNode)
					(make-left-variable-pair L-ATOM R-ATOM))
				((equal? (cog-type R-ATOM) 'VariableNode)
					(make-right-variable-pair L-ATOM R-ATOM))
				(else (make-ordinary-pair L-ATOM R-ATOM))))

		; Given a pair, find the left element in it.
		(define (get-pair-left PAIR)
			(init-a-set)
			; If PAIR is a wildcard, then extract the wild-left.
			; We used disjoint-left to construct it, so use same to test it.
			(if (and (not disjoint-left)
					(equal? 'EvaluationLink (cog-type PAIR))
					(equal? pred-node (cog-outgoing-atom PAIR 0)))
				(cog-outgoing-atom PAIR 1)

				; It's not a wild-card. Delegate.
				(if (is-from-a? PAIR)
					(LLA 'left-element PAIR)
					(LLB 'left-element PAIR))))

		; Given a pair, find the right element in it.
		(define (get-pair-right PAIR)
			(init-a-set)
			; If PAIR is a wildcard, then extract the wild-right.
			; We used disjoint-right to construct it, so use same to test it.
			(if (and (not disjoint-right)
					(equal? 'EvaluationLink (cog-type PAIR))
					(equal? pred-node (cog-outgoing-atom PAIR 0)))
				(cog-outgoing-atom PAIR 2)

				; It's not a wild-card. Delegate.
				(if (is-from-a? PAIR)
					(LLA 'right-element PAIR)
					(LLB 'right-element PAIR))))

		; Return the count on the pair, if it exists.
		(define (get-pair-count L-ATOM R-ATOM)
			(define maybe-a (LLA 'get-pair L-ATOM R-ATOM))
			(if (not (nil? maybe-a))
				(LLA 'get-count maybe-a)
				(LLB 'pair-count L-ATOM R-ATOM)))

		; Return the count on the pair, by delegating.
		; Maintains a cache of all atoms in LLA, and uses that
		; to delegate.
		(define (get-count PAIR)
			(init-a-set)
			(if (is-from-a? PAIR)
				(LLA 'get-count PAIR) (LLB 'get-count PAIR)))

		; As above, but the setter.
		(define (set-count PAIR CNT)
			(init-a-set)
			(if (is-from-a? PAIR)
				(LLA 'set-count PAIR CNT) (LLB 'set-count PAIR CNT)))

		; Delegate wildcards to the two components.
		; The disjoint side gives us an unambiguous wildcard that
		; we can delegate to. This works safely because 'filters?
		; is #t and so marginals are labelled with the 'id of this
		; object. The non-disjoint side requires a custom wildcard.
		; The hope here is that we save a little bit of RAM by
		; recycling existing wildcards on the disjoint side.
		; Otherwise, we can just use custom wildcards for both sides.
		(define (left-wildcard R-ATOM)
			(init-a-base)
			(if disjoint-right
				(if (type-a? R-ATOM R-ATOM)
					(LLA 'left-wildcard R-ATOM)
					(LLB 'left-wildcard R-ATOM))
				(EvaluationLink pred-node left-wnode R-ATOM)))

		(define (right-wildcard L-ATOM)
			(init-a-base)
			(if disjoint-left
				(if (type-a? L-ATOM L-ATOM)
					(LLA 'right-wildcard L-ATOM)
					(LLB 'right-wildcard L-ATOM))
				(EvaluationLink pred-node L-ATOM right-wnode)))

		(define (get-wild-wild)
			(EvaluationLink pred-node left-wnode right-wnode))

		; ===================================================
		; Overloaded stars functions

		(define (union MSG)
			(define atom-set (make-atom-set))
			(for-each atom-set (a-stars MSG))
			(for-each atom-set (b-stars MSG))
			(atom-set #f))

		(define (left-basis)
			(if (null? l-basis) (set! l-basis (union 'left-basis)))
			l-basis)

		(define (left-basis-size)
			(if (eq? 0 l-size) (set! l-size (length (left-basis))))
			l-size)

		(define (right-basis)
			(if (null? r-basis) (set! r-basis (union 'right-basis)))
			r-basis)

		(define (right-basis-size)
			(if (eq? 0 r-size) (set! r-size (length (right-basis))))
			r-size)

		(define (gunion MSG ARG)
			(define atom-set (make-atom-set))
			(for-each atom-set (a-stars MSG ARG))
			(for-each atom-set (b-stars MSG ARG))
			(atom-set #f))

		; Stars and duals are unions.
		(define (left-stars R-ATOM)
			(gunion 'left-stars R-ATOM))

		(define (left-duals R-ATOM)
			(gunion 'left-duals R-ATOM))

		(define (right-stars L-ATOM)
			(gunion 'right-stars L-ATOM))

		(define (right-duals L-ATOM)
			(gunion 'right-duals L-ATOM))

		; Just get all the parts, and append them.
		; Avoid a cpu-sucking union. Thus, any elts appearing in
		; both will be dpulicated. This may or may not be what the
		; user expects. It's up to the user to de-dupe if that's
		; what they want.
		(define (get-all-elts)
			(append (a-stars 'get-all-elts) (b-stars 'get-all-elts)))

		; This is needed, as the current code base delete members in
		; the matrix out from under it. So we use 'clobber to invalidate
		; counts.
		(define (clobber)
			(a-stars 'clobber)
			(b-stars 'clobber)

			(set! l-basis '())
			(set! r-basis '())
			(set! l-size 0)
			(set! r-size 0)
			(set! is-from-a? #f)
			(set! type-a? #f)
			(set! in-base? #f)
		)

		; -------------
		;
		; Here's the deal: if one, but not the other object provides
		; the method, just call it. If both provide it, then we are
		; confused, and throw an error. If neither provide it, that's
		; also an error.
		(define (catch-all message args)
			(define method-on-a (LLA 'provides message))
			(define method-on-b (LLB 'provides message))
			(cond
				((and method-on-a (not method-on-b)) (apply method-on-a args))
				((and method-on-b (not method-on-a)) (apply method-on-b args))
				(else (throw 'bad-use 'direct-sum
							(format #f "Sorry, method `~A` on ~A not available!"
								message id-string)))))

		; -------------
		(define (help)
			(format #t
				(string-append
"This is the `direct sum` of '~A' and '~A'\n"
"It takes the union of the left-basis of both objects, and likewise\n"
"the right-basis. For more information, say `,d diect-sum` or\n"
"`,describe direct-sum` at the guile prompt, or just use the 'describe\n"
"method on this object.\n"
)
				(LLA 'id) (LLB 'id)))

		(define (describe)
			(display (procedure-property direct-sum 'documentation)))

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

				(else               #f)
			))

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((name)             (get-name))
				((id)               (get-id))
				((left-type)        (get-left-type))
				((right-type)       (get-right-type))
				((pair-type)        (get-pair-type))

				((get-pair)         (apply get-pair args))
				((pair-count)       (apply get-pair-count args))
				((get-count)        (apply get-count args))
				((set-count)        (apply set-count args))
				((make-pair)        (apply make-pair args))
				((left-element)     (apply get-pair-left args))
				((right-element)    (apply get-pair-right args))
				((left-wildcard)    (apply left-wildcard args))
				((right-wildcard)   (apply right-wildcard args))
				((wild-wild)        (get-wild-wild))
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
				(else               (catch-all message args))
	)))
)

; ---------------------------------------------------------------------
