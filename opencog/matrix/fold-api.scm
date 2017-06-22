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
; treating each disjunct as a basis element, take thier difference, and
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
(use-modules (opencog))

; ---------------------------------------------------------------------
;
(define*-public (add-tuple-math LLOBJ FUNC #:optional
	(GET-CNT 'pair-count))
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

  The 'item-pair method just distributes over the tuples. In the
  above example, 'item-pair [(x,y), (x,z), (x,w)] will return the
  triple ['item-pair (x,y), 'item-pair (x,z), 'item-pair (x,w)]

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
			(delete-duplicates!
				(append-map!
					(lambda (item) (map! gar (star-obj 'left-stars item)))
					TUPLE)))

		(define (get-right-union TUPLE)
			(delete-duplicates!
				(append-map!
					(lambda (item) (map! gdr (star-obj 'right-stars item)))
					TUPLE)))

		; ---------------
		; Given a TUPLE of items of 'right-type, this returns
		; a tuple of low-level pairs of LEFTY and each righty
		; in the TUPLE.  If such a pair does not exist, the
		; returned tuple will contain an empty list at that locus.
		(define (get-left-lopr-tuple LEFTY TUPLE)
			(define prty (LLOBJ 'pair-type))
			(map
				(lambda (rght) (cog-link prty LEFTY rght))
				TUPLE))

		(define (get-right-lopr-tuple RIGHTY TUPLE)
			(define prty (LLOBJ 'pair-type))
			(map
				(lambda (left) (cog-link prty left RIGHTY))
				TUPLE))

		; ---------------
		; Expects TUPLE to be a scheme list of items of 'right-type.
		; Returns a list of tuples of the left-stars appropriae for
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
		;          '()))  ; Note the par zed-a does not exist.
		;
		; In the above, the union of the left support was {foo, bar, zed}
		; and the intersection of the left support was just {foo}.  This
		; will be the typical case: the intersection will be typically
		; non-empty, and the union will typically be strictly larger.

		(define (left-star-union TUPLE)
			(map
				(lambda (lefty) (get-left-lopr-tuple lefty TUPLE))
				(get-left-union TUPLE)))

		; Same as above, but for the right
		(define (right-star-union TUPLE)
			(map
				(lambda (righty) (get-right-lopr-tuple righty TUPLE))
				(get-right-union TUPLE)))

		; ---------------
		; Given a TUPLE of low-level pairs, return a tuple of high-level
		; pairs.
		(define (get-pair TUPLE)
			(map (lambda (lopr) (LLOBJ 'item-pair lopr)) TUPLE))

		; Given a TUPLE of high-level pairs, return a single number.
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
				((item-pair)   get-pair)
				((pair-count)  get-func-count)
				(else          (LLOBJ 'provides meth))))

		; ---------------

		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-stars)      (apply left-star-union args))
				((right-stars)     (apply right-star-union args))
				((item-pair)       (apply get-pair args))
				((pair-count)      (apply get-func-count args))
				((provides)        (apply provides args))
				(else              (apply LLOBJ (cons message args))))
			)))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
