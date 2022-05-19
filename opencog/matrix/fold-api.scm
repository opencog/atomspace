;
; fold-api.scm
;
; Provides support for folding down tuples of pairs.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; There is sometimes a need to take the difference or sum, min or max,
; or maybe the product of two different rows or columns in a matrix.
; This provides the API to do that.
;
; So, for example, say we have a matrix N(x,y) of pairs, and want to
; apply some function F taking two arguments, and apply this function
; to two rows or columns.  The API here creates a new "virtual" matrix
; holding values
;
;      N(x, [y,z]) = F(N(x,y), N(x,z))
;
; when addressed by columns, and
;
;      N([u,x], y) = F(N(u,y), N(x,y))
;
; when address by rows. Here, F computes the sum, difference, min, max,
; etc. as desired.
;
; For example, to take differences of rows or columns of the
; connector-set matrix, do this:
;
;      (define vecty (make-pseudo-cset-api)
;      (define subby (add-tuple-math vecty -))
;      (define normy (add-support-compute subby))
;
; The length of the difference of two rows can then be computed as:
;
;      (normy 'right-length (list (Word "the") (Word "a")))
;
; which will take the disjunct-vectors for the two words "the" and "a",
; treating each disjunct as a basis element, take their difference, and
; return the length (the root-mean-square of the difference of the
; counts).
;
; There are other possibilities.  To compute the number of disjuncts
; that these two words have in common, define a set-intersection
; function:
;
;    (define (intersect A-WORD-CNT B-WORD-CNT)
;       (if (and (< 0 A-WORD-CNT) (< 0 B-WORD-CNT)) 1 0))
;
; then
;
;      (define isect (add-tuple-math vecty intersect))
;      (define secty (add-support-compute isect))
;      (secty 'right-count (list (Word "the") (Word "a")))
;
; will return how many disjuncts there are shared, in common, with both
; of these words.  Similarly, the set union will count how many
; disjuncts are used, between the two:
;
;    (define (union A-WORD-CNT B-WORD-CNT)
;       (if (or (< 0 A-WORD-CNT) (< 0 B-WORD-CNT)) 1 0))
;
; then
;
;      (define youny (add-tuple-math vecty union))
;      (define uniny (add-support-compute youny))
;      (uniny 'right-count (list (Word "the") (Word "a")))
;
; returns the number of disjuncts that appear in either the word "the"
; or in the word "a".
;
; The function provided to add-tuple-math can be any function taking
; a tuple of numbers, and returning a single number. The only constraint
; is that the arity of the function must match the arity of the tuple
; provided to the stars methods.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog exec))

; ---------------------------------------------------------------------
;
(define*-public (add-tuple-math LLOBJ FUNC #:optional
	(GET-CNT 'get-count))
"
  add-tuple-math LLOBJ FUNC - Extend LLOBJ with ability to take tuples
  of rows or columns, and then call FUNC on that tuple, in the place of
  a regular call to the 'pair-count method. This creates a 'virtual'
  matrix whose entries are a function FUNC of the rows or columns of
  the original matrix. So, for example, given that LLOBJ holds a matrix
  N(x,y) of pairs, and FUNC taking three arguments, then this object
  creates a new matrix holding values

      N(x, [y,z,w]) = FUNC(N(x,y), N(x,z), N(x,w))

  Typically, FUNC takes only two rows or columns, and returns the sum
  or the difference, the min or the max, or the product of these two
  rows or columns.

  The basic trick is accomplished by overloading the 'left-stars and
  'right-stars methods to take tuples of rows or columns, and then
  return a tuple of matrix entries.

  For example, given three columns [y,z,w], the 'left-stars method
  will return the set

     { [(x,y), (x,z), (x,w)] | at least one of (x,y), (x,z), (x,w)
                               exists in the atomspace. }
  Here, [(x,y), (x,z), (x,w)] is a triple of matrix entries,
  specifically, three matrix entries for the same row but three
  different columns.

  If one of the matrix entries does not exist, it is replaced by
  nil in the tuple.  So, for the above example, if (x,y) did not exist,
  then the returned triple would be [(), (x,z), (x,w)].  At least one
  of the entries in the tuple is non-nil.

  The 'get-pair method just distributes over the tuples. In the
  above example, 'get-pair [(x,y), (x,z), (x,w)] will return the
  triple ['get-pair (x,y), 'get-pair (x,z), 'get-pair (x,w)]

  The 'pair-count method will apply FUNC to the tuple. In the above
  example, the 'pair-count method will return a number, specifically,
  the single number
     FUNC('pair-count (x,y), 'pair-count (x,z), 'pair-count (x,w))

  The function FUNC must take a tuple of numbers, and return a single
  number as a result. The function FUNC must be able to take the same
  number of arguments as the arity of the tuple.

  To simplify the usage, an optional argument can be provided: the
  name of a method to use, instead of 'pair-count, for obtaining
  numeric values. A typical example would be to use 'pair-freq
  to get the frequency p(x,y) of a pair, instead of the count N(x,y).
"
	(let ((star-obj (add-pair-stars LLOBJ))
			(get-cnt (lambda (x) (LLOBJ GET-CNT x)))
		)

		; ---------------
		; Return the set-union of all atoms that might be paired
		; with one of the atoms from TUPLE on the right.
		(define (get-left-union TUPLE)
			; The set the will hold the union of atoms.
			; The atom-set guarantees that each item will
			; appear only once in the set, even if it is
			; inserted repeatedly.
			(define atom-set (make-atom-set))

			; Loop over everything in the tuple.
			(for-each
				(lambda (item)
					(for-each (lambda (dual) (atom-set dual))
						(star-obj 'left-duals item)))
				TUPLE)

			; Return the union of all left-stars in the tuple.
			(atom-set #f))

		(define (get-right-union TUPLE)
			(define atom-set (make-atom-set))
			(for-each
				(lambda (item)
					(for-each (lambda (dual) (atom-set dual))
						(star-obj 'right-duals item)))
				TUPLE)

			(atom-set #f))

		; ---------------
		; Given a TUPLE of items of 'right-type, this returns
		; a tuple of pairs of LEFTY and each righty in the TUPLE.
		; If such a pair does not exist, the returned tuple will
		; contain an empty list at that locus.
		(define (get-left-tuple LEFTY TUPLE)
			(map
				(lambda (rght) (LLOBJ 'get-pair LEFTY rght))
				TUPLE))

		(define (get-right-tuple ROW-TUPLE COL)
			(map
				(lambda (row) (LLOBJ 'get-pair row COL))
				ROW-TUPLE))

		; ---------------
		; Expects TUPLE to be a scheme list of items of 'right-type.
		; That is, TUPLE is a list of columns.
		; Returns a list of tuples of the left-stars appropriate for
		; that TUPLE.  The left-star tuples are "aligned", so that
		; that within one tuple, all pairs have exactly the same
		; left side.  If such a pair does not exist, the empty list
		; stands in its place.  The only case where this would not
		; happen is if all items in the TUPLE had exactly the same
		; left-wilds.  But this would be a very unusual thing, in the
		; normal case.
		;
		; So, for example, if TUPLE is (list (Word "the") (Word "a"))
		; This might return a list of say, 3 tuples:
		;   (list
		;      (list  ; Note left atoms are identical.
		;          (ListLink (Word "foo") (Word "the"))
		;          (ListLink (Word "foo") (Word "a")))
		;      (list
		;          '()  ; Note the pair bar:the does not exist
		;          (ListLink (Word "bar") (Word "a")))
		;      (list
		;          (ListLink (Word "zed") (Word "the"))
		;          '()))  ; Note the pair zed-a does not exist.
		;
		; In the above, the union of the left support was {foo, bar, zed}
		; and the intersection of the left support was just {foo}.  This
		; will be the typical case: the intersection will be typically
		; non-empty, and the union will typically be strictly larger.

		(define (left-star-union COL-TUPLE)
			(map
				(lambda (lefty) (get-left-tuple lefty COL-TUPLE))
				(get-left-union COL-TUPLE)))

		; Same as above, but for the right. ROW-TUPLE is a list of rows.
		; `get-right-union` returns a set of columns.
		(define (right-star-union ROW-TUPLE)
			(map
				(lambda (col) (get-right-tuple ROW-TUPLE col))
				(get-right-union ROW-TUPLE)))

		; ---------------
		; Given a TUPLE of pairs, return a single number.
		; The FUNC is applied to reduce the counts on each pair
		; in the tuple down to just one number.
		(define (get-func-count TUPLE)
			(apply FUNC
				(map
					(lambda (pr) (if (null? pr) 0 (get-cnt pr)))
					TUPLE)))

		; ---------------
		; Return a pointer to each method that this class overloads.
		(define (provides meth)
			(case meth
				((left-stars)  left-star-union)
				((right-stars) right-star-union)
				((get-count)   get-func-count)
				(else          (LLOBJ 'provides meth))))

		; ---------------

		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-stars)      (apply left-star-union args))
				((right-stars)     (apply right-star-union args))
				((get-count)       (apply get-func-count args))
				((provides)        (apply provides args))
				(else              (apply LLOBJ (cons message args))))
			)))

; ---------------------------------------------------------------------
;
(define*-public (add-fast-math LLOBJ FUNC #:optional
	(GET-CNT 'get-count))
"
  add-fast-math LLOBJ FUNC - Fast version of `add-tuple-math`

  See `add-tuple-math` for details. This is much faster, as it uses
  the pattern engine to find tuples. It is limited, though: it will
  only return intersections!  This is sufficient for taking products
  but is not enough for sums.

  For example, given two columns [y,z], the 'left-stars method
  will return the set

     { [(x,y), (x,z)] | both (x,y) and (x,z) are present in
                       the atomspace. }
"
	(let ((star-obj (add-pair-stars LLOBJ))
			(get-cnt (lambda (x) (LLOBJ GET-CNT x)))
		)
		; ---------------
		(define (thunk-type TY) (if (symbol? TY) (TypeNode TY) TY))

		; ---------------
		(define (left-star-intersect COL-TUPLE)
			(define row-var (uniquely-named-variable)) ; shared rows
			(define row-type (thunk-type (LLOBJ 'left-type)))
			(define term-list
				(map (lambda (COL) (LLOBJ 'make-pair row-var COL)) COL-TUPLE))

			(define qry
				(Meet
					(TypedVariable row-var row-type)
					(Present term-list)))

			(define rowset (cog-value->list (cog-execute! qry)))
			(cog-extract-recursive! row-var)

			; Convert what the pattern engine returned to
			; a list of scheme lists.
			(map
				(lambda (ROW)
					(map (lambda (COL) (LLOBJ 'make-pair ROW COL)) COL-TUPLE))
				rowset)
		)

		; ---------------
		(define (right-star-intersect ROW-TUPLE)
			(define col-var (uniquely-named-variable)) ; shared cols
			(define col-type (thunk-type (LLOBJ 'right-type)))
			(define term-list
				(map (lambda (ROW) (LLOBJ 'make-pair ROW col-var)) ROW-TUPLE))

			; XXX TODO -- It is MUCH faster to have the qry create the
			; desired pairs for us. However, this is problematic for
			; the direct-sum, where the top query term is a ChoiceLink,
			; and there is no easy way to get which choice was made!
			; So we pay a fairly hefty penalty running the map immediately
			; below. It would be better to do it all in the pattern engine.
			; Even better: run the FUNC on everything and return that.
			; Better still: run the FUNC on the query results, in the C++
			; code, and not in scheme.
			(define qry
				(Meet
					(TypedVariable col-var col-type)
					(Present term-list)))

			(define colset (cog-value->list (cog-execute! qry)))
			(cog-extract-recursive! col-var)

			; Convert what the pattern engine returned to
			; a list of scheme lists.
			(map
				(lambda (COL)
					(map (lambda (ROW) (LLOBJ 'make-pair ROW COL)) ROW-TUPLE))
				colset)
		)

		; ---------------
		; Given a TUPLE of pairs, return a single number.
		; The FUNC is applied to reduce the counts on each pair
		; in the tuple down to just one number.
		(define (get-func-count TUPLE)
			(apply FUNC
				(map
					(lambda (pr) (if (null? pr) 0 (get-cnt pr)))
					TUPLE)))

		; ---------------
		; Return a pointer to each method that this class overloads.
		(define (provides meth)
			(case meth
				((left-stars)  left-star-intersect)
				((right-stars) right-star-intersect)
				((get-count)   get-func-count)
				(else          (LLOBJ 'provides meth))))

		; ---------------

		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-stars)      (apply left-star-intersect args))
				((right-stars)     (apply right-star-intersect args))
				((get-count)       (apply get-func-count args))
				((provides)        (apply provides args))
				(else              (apply LLOBJ (cons message args)))))
	)
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
