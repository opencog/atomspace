;
; object-api.scm
;
; Define object-oriented class API's for correlation matricies of atoms.
;
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; In this project, there's a generic theme of "pairs of things" that
; are statistically related. These can be pairs of words, they can be
; connector-sets, which are a pair of (word, disjunct), or they can
; be other things.
;
; More precisely, we are generally interested in pairs (x,y) of atoms
; (that is, where x and y are atoms), and we have some sort of count
; N(x,y) of how often that particular pair was observed.  We typically
; are then interested in various statistical measures: usually starting
; with the normalized frequency (probability, likelihood) of how often
; the pair (x,y) occured,
;
; For all of these pairs (x,y), we typically need to get the count
; N(x,y), the partial sums N(x,*) = sum_y N(x,y), and likewise N(*,y)
; and N(*,*).   We need to compute frequencies of observations, such
; as p(x,y) = N(x,y)/N(*,*).  We also need to compute entropies and
; mutual information, which can be infered from these frequencies.
; We also can compute cosine-similairy and other metrics of similarity,
; dervied solely from the observed frequency counts.
;
; All of these formulas are independent of the actual objects in the
; pairs.  Thus, it is useful to separae the various algorithms from
; the data that they operate on. Towards this end, this file defines
; some object-oriented OO API's for pairs, which the algos can assume,
; and the different types of pairs can implement.
;
; The object-system being used here is a roll-your-own type system,
; really quite simple, as it's well-suited for the desired task.
; It's simple, and minimal. The reasons for this are explained here:
;    http://community.schemewiki.org/?object-oriented-programming
; Basically, "object-oriented programming" is a mish-mash of more
; than half-a-dozen distinct concepts, almost all of which are
; not needed for this particular project.  The only thing we need
; is the ability to decorate objects with additional methods, kind-of
; like class inheritance, except that we really really need dynamic
; inheritance, i.e. arbitrary base classes, rather than a single,
; static base class, and so its totally unlike C++ inheritance, which
; is static, and a lot like C++ templates, which are dynamic.
; Basically, what is needed, and what is implemented here is called
; "parametric polymorphism".
;
; From what I can tell, tiny-CLOS and thus GOOPS does not support
; parametric polymorphism... !?? and so I go it alone. The system here
; is really really simple...
;
; The object system here is almost identical to this one:
;    http://community.schemewiki.org/?simple-object
; Read this URL to understand what is happening here.
;
; There are several API's here. The lowest-level ones are listed first.
;
; XXX FIXME ... the calling seuqence is exactly backeards. In order
; for overloading to work correct, attempts must be made to call
; methods on the base object first, and only later on the wrapper.
; For now, we blow this off, but in the long run, this needs to be
; fixed.
;
; ---------------------------------------------------------------------
;
; Example low-level API class. It has only six methods; these
; return pair-atoms on which counts are stored as values.
; Higher-evel objects use this object to fetch counts, store them
; into the database, or to return various statistics.
;
; The `make-pair-count-get-set` class, below, is a typical user
; of this class; it provides getters and setters for the counts.
;
; See `make-any-link-api` for a working example.
;
; When called, this will create a new instance of the class
; i.e. will create a new object.
;
;  (define (make-ll-object-api-example)
;
;     ; Return the atom-type of the matrix rows, i.e. the type of
;     ; the left side of the (row, column) pairs.
;     (define (get-left-type) 'WordNode)
;
;     ; Return the atom-type of the matrix columns, i.e. the type of
;     ; the right side of the (row, column) pairs.
;     (define (get-right-type) 'WordNode)
;
;     ; Return the type of the pair itself.  In this example, each pair
;     ; will be of the form (ListLink (Word "row") (Word "col"))
;     (define (get-pair-type) 'ListLink)
;
;     ; Return the atom for a matrix (row,column) pair, if it exists,
;     ; else return nil. In this example, the matrix is defined by an
;     ; EvaluationLink holding the ListLink. This atom is where all
;     ; values associated with this matrix are held.  This includes not
;     ; only the count (the number of observations of the pair) but also
;     ; any dervides values, such as frequency, mutual information, and
;     ; so on. Users are free to (are encouraged to) use this atom to
;     ; attach additional information and statistics.
;     ;
;     ; The PAIR atom must be of 'pair-type, that is, a ListLink in this
;     ; example.  Note: the cog-link function does NOT create the atom,
;     ; if it does not already exist!
;     (define (get-pair PAIR)
;        (cog-link 'EvaluationLink (Predicate "foo") PAIR))
;
;     ; Return the observed count for PAIR, if it exists, else
;     ; return zero. The PAIR atom must be of type 'pair-type;
;     ; that is, a ListLink in this example.
;     (define (get-pair-count PAIR)
;        (define stats-atom (get-pair PAIR))
;        (if (null? stats-atom) 0
;           (cog-value-ref
;               (cog-value stats-atom (Predicate "counter")) 42)))
;
;     ; Return the atom holding the count, creating it if it does
;     ; not yet exist.  Returns the same structure as the 'item-pair
;     ; method (the get-pair function, above).
;     (define (make-pair PAIR)
;        (EvaluationLink (Predicate "foo") PAIR))
;
;     ; Return an atom to which column subtotals can be attached,
;     ; such as, for example, the subtotal `N(*,y)`. Thus, `y`
;     ; denotes a column, and the star is on the left (the star
;     ; ranging over all rows).
;     (define (get-left-wildcard ITEM)
;        (EvaluationLink (Predicate "foo")
;           (ListLink (AnyNode "left-wild") ITEM)))
;
;     ; Return an atom to which row subtotals can be attached,
;     ; such as, for example, the subtotal `N(x,*)`. Thus, `x`
;     ; denotes a row, and the star is on the right (the star
;     ; ranging over all columns).
;     (define (get-right-wildcard ITEM)
;        (EvaluationLink (Predicate "foo")
;           (ListLink ITEM (AnyNode "right-wild"))))
;
;     ; Return an atom to which matrix totals can be attached,
;     ; such as, for example, the total `N(*,*)`. This can be any
;     ; atom, but must be unique to the specific matrix. It's
;     ; convenient to use the same style as the subtotals.
;     (define (get-wild-wild)
;        (EvaluationLink (Predicate "foo")
;           (ListLink (AnyNode "left-wild") (AnyNode "right-wild"))))
;
;     ; Retrieve, from storage, the entire matrix, including the
;     ; subtotal and total anchor atoms.  In this example, its enough
;     ; to get the incoming set of (Predicate "foo"), but this need
;     ; not generally be the case.
;     (define (fetch-all-pairs)
;        (fetch-incoming-by-type (Predicate "foo") 'EvaluationLink))
;
;     ; Methods on the class. To call these, quote the method name.
;     ; Example: (OBJ 'left-wildcard WORD) calls the
;     ; get-left-wildcard function, passing WORD as the argument.
;     ;
;     ; The name is a string printed at the top of generated reports.
;     ; The id is a short string used to create unique filter ids and names.
;     (lambda (message . args)
;        (apply (case message
;              ((name) "A Kind of Demonstration Object")
;              ((id)   "demo")
;              ((left-type) get-left-type)
;              ((right-type) get-right-type)
;              ((pair-type) get-pair-type)
;              ((pair-count) get-pair-count)
;              ((item-pair) get-pair)
;              ((make-pair) make-pair)
;              ((left-wildcard) get-left-wildcard)
;              ((right-wildcard) get-right-wildcard)
;              ((wild-wild) get-wild-wild)
;              ((fetch-pairs) fetch-all-pairs)
;              ((provides) (lambda (symb) #f))
;              ((filters?) (lambda () #f))
;              (else (error "Bad method call on low-level API")))
;           args)))
;
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog))

; ---------------------------------------------------------------------

(define-public (add-pair-stars LLOBJ)
"
  add-pair-stars LLOBJ - Extend LLOBJ with row and column access
  methods (aka wildcard methods); specifically, to get all non-zero
  elements in a given row or column.

  The supported methods are:
  'left-basis - Return all items (atoms) that can be used to index
      a row in the matrix.  That is, given a matrix N(x,y), this
      returns the set {x | (x,y) exists in the atomspace for some y}.
      All of the elements of this set will be atoms of type
      (LLOBJ 'left-type).  A check is made to verify that (x,y) is
      a valid pair, viz that it is an atom whose type is
      (LLOBJ 'pair-type) and that y is of type (LLOBJ 'right-type).
      This only verifies that such pairs exist in the atomspace; it
      does NOT verify that they have a nonzero count!

  'right-basis - Likewise, but for columns.

  'left-basis-size - the size of the 'left-basis set; i.e. the number
      of unique, distinct atoms in that set.

  'right-basis-size - Likewise.

  'left-stars COL - Return the set of pairs (row, column) for
      which the column is COL, and the pair exists in the atomspace.
      That is, return the set
         (*, COL) == { (x,COL) | (x,COL) exists in the atomspace }
      The returned pairs will all be of type (LLOBJ 'pair-type),
      and the x's will all be of type (LLOBJ 'left-type). The
      input COL atom must be of type (LLOBJ 'right-type).  This does
      NOT verify that these pairs have a non-zero count.

  'right-stars ROW - Likewise, but returns the set (ROW, *).

  Here, the LLOBJ is expected to be an object, with methods for
  'left-type, 'right-type and 'pair-type on it. It is assumed that
  the pairs are arity-two links having the form
     (pair-type (left-type right-type))
  That is, the pair-type is the low-level pair type.
"
	(let ((l-basis '())
			(r-basis '())
			(l-size 0)
			(r-size 0)
			(pair-type (LLOBJ 'pair-type))
			(left-type (LLOBJ 'left-type))
			(right-type (LLOBJ 'right-type))
		)

		; Return a list of all atoms of TYPE which appear in a Link
		; of type 'pair-type
		(define (get-basis TYPE PAIR-FILT)
			(remove!
				(lambda (item)
					(null? (PAIR-FILT item (cog-incoming-by-type item pair-type))))
				(cog-get-atoms TYPE)))

		; Given a left-item, and a list of pairs, return only those
		; pairs in which the left-item really is on the left, and
		; the correct type is on the right.
		(define (good-right-pairs left-item pair-list)
			(filter!
				(lambda (pr)
					(and
						(eq? 2 (cog-arity pr))
						(equal? left-item (gar pr))
						(eq? right-type (cog-type (gdr pr)))))
				pair-list))

		(define (good-left-pairs right-item pair-list)
			(filter!
				(lambda (pr)
					(and
						(eq? 2 (cog-arity pr))
						(eq? left-type (cog-type (gar pr)))
						(equal? right-item (gdr pr))))
				pair-list))

		; Return a list of all of the atoms that might ever appear on
		; the left-hand-side of a pair.  This is the set of all possible
		; items x from the pair (x,y) for any y.
		;
		; XXX FIXME ... the good-right-pairs check is extremely
		; CPU-expensive! Its currently taking 3 minutes to get the
		; list of words!
		;
		(define (get-left-basis)
			(if (null? l-basis)
				(set! l-basis (get-basis left-type good-right-pairs)))
			l-basis)

		(define (get-right-basis)
			(if (null? r-basis)
				(set! r-basis (get-basis right-type good-left-pairs)))
			r-basis)

		(define (get-left-size)
			(if (eq? 0 l-size) (set! l-size (length (get-left-basis))))
			l-size)

		(define (get-right-size)
			(if (eq? 0 r-size) (set! r-size (length (get-right-basis))))
			r-size)

		;-------------------------------------------
		; Return a list of all pairs with the ITEM on the right side,
		; and an object of type (LLOBJ 'left-type) on the left.
		; The point of this function is to find and return the complete
		; wild-card set (*,y) = {(x,y) | there-exists pair (x,y) for some x}
		; The pairs are Links of type 'pair-type, of arity two. That is,
		; this returns a list of atoms of the form
		;
		;    (cog-link (LLOBJ 'pair-type)
		;         (cog-atom (LLOBJ 'left-type) ...)
		;         ITEM)
		;
		; ITEM should be an atom of (LLOBJ 'right-type); if it isn't,
		; the the behavior is undefined.
		;
		; Currently, this implementation always goes back to the
		; atomspace, and re-filters the results eaach time.  Some
		; performance could be gained, at the expense of greater
		; memory usage, by using the atom cache to save these results.
		;
		(define (get-left-stars ITEM)
			(filter
				(lambda (lnk)
					(define oset (cog-outgoing-set lnk))
					(and
						(equal? 2 (cog-arity lnk))
						(equal? left-type (cog-type (first oset)))
						(equal? ITEM (second oset))
					))
				(cog-incoming-by-type ITEM pair-type)))

		; Same as above, but on the right.
		(define (get-right-stars ITEM)
			(filter
				(lambda (lnk)
					(define oset (cog-outgoing-set lnk))
					(and
						(equal? 2 (cog-arity lnk))
						(equal? ITEM (first oset))
						(equal? right-type (cog-type (second oset)))
					))
				(cog-incoming-by-type ITEM pair-type)))

		;-------------------------------------------
		; Return default, only if LLOBJ does not provide symbol
		(define (overload symbol default)
			(define fp (LLOBJ 'provides symbol))
			(if fp fp default))

		; Provide default methods, but only if the low-level object
		; does not already provide them. In practice, this is used in
		; two different ways: One is by the fold-api which overloads
		; the stars methods to work differently.  The other is to
		; allow an underlying object to provide cached basis, as these
		; can sometimes take an obscenely long time to compute.
		(let ((f-left-basis (overload 'left-basis get-left-basis))
				(f-right-basis (overload 'right-basis get-right-basis))
				(f-left-basis-size (overload 'left-basis-size get-left-size))
				(f-right-basis-size (overload 'right-basis-size get-right-size))
				(f-left-stars (overload 'left-stars get-left-stars))
				(f-right-stars (overload 'right-stars get-right-stars)))

			;-------------------------------------------
			; Explain what it is that I provide. The point here is that
			; computing the left and right basis can be very expensive,
			; and so if it has already been computed, that should be used,
			; by defering to the object that holds those caches. We do
			; by advertising that .. we hold caches.
			(define (provides meth)
				(case meth
					((left-stars)       f-left-stars)
					((right-stars)      f-right-stars)
					((left-basis)       f-left-basis)
					((right-basis)      f-right-basis)
					((left-basis-size)  f-left-basis-size)
					((right-basis-size) f-right-basis-size)
					(else               (LLOBJ 'provides meth))))

			;-------------------------------------------
			; Methods on this class.
			(lambda (message . args)
				(case message
					((left-basis)       (f-left-basis))
					((right-basis)      (f-right-basis))
					((left-basis-size)  (f-left-basis-size))
					((right-basis-size) (f-right-basis-size))
					((left-stars)       (apply f-left-stars args))
					((right-stars)      (apply f-right-stars args))
					((provides)         (apply provides args))
					(else               (apply LLOBJ (cons message args))))
			))))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------

(define*-public (add-pair-count-api LLOBJ
    #:optional (ID (LLOBJ 'id)))
"
  add-pair-count-api LLOBJ ID - Extend LLOBJ with count-getters.

  Extend the LLOBJ with additional methods to get and set
  marginal counts (subtotal wild-card counts), and total counts.
  Basically, this decorates the class with additional methods
  that get and set these counts in \"standardized\" places.
  Other classes can overload these methods; these just provide
  a reasonable default.

  The optional ID argument should be #f or a string, used to construct
  the key under which the values are stored.

  If the dataset is not filtered, the counts are stored in the
  CountTruthValue assocaited with the atom; else they are stored
  in a value specific to the filter-id.

  These methods do NOT compute the counts! They merely provide fast
  access to values that were previously computed and stored in the
  atomspace.  A method is provided to set the value, as well.

  Here, the LLOBJ is expected to be an object, with methods for
  'item-pair 'make-pair 'left-wildcard 'right-wildcard and 'wild-wild
  on it, in the form documented above for the \"low-level API class\".
"
	; ----------------------------------------------------
	; Key under which the count values are stored.
	(define is-filtered? (and ID (LLOBJ 'filters?)))

	(define cnt-name (string-append "*-CountKey " ID))

	(define cnt-key (PredicateNode cnt-name))

	; Return the count on ATOM. Use the CountTruthValue if not
	; filtered, else use the CountKey predicate.
	(define (get-count ATOM)
		(if (null? ATOM) 0
			(if is-filtered?
				(cog-value-ref (cog-value ATOM cnt-key) 0)
				(cog-tv-count (cog-tv ATOM)))))

	; Set a count on the ATOM.
	(define (set-count ATOM CNT)
		(if is-filtered?
			(cog-set-value! ATOM cnt-key (FloatValue CNT))
			(cog-set-tv! ATOM (cog-new-ctv 0 0 CNT))))

	; Get the left wildcard count
	(define (get-left-wild-count ITEM)
		(get-count (LLOBJ 'left-wildcard ITEM)))

	; Get the right wildcard count
	(define (get-right-wild-count ITEM)
		(get-count (LLOBJ 'right-wildcard ITEM)))

	; Set the left wildcard count
	; Return the atom that holds this count.
	(define (set-left-wild-count ITEM CNT)
		(set-count (LLOBJ 'left-wildcard ITEM) CNT))

	; Set the right wildcard count
	; Return the atom that holds this count.
	(define (set-right-wild-count ITEM CNT)
		(set-count (LLOBJ 'right-wildcard ITEM) CNT))

	; Get the wildcard-wildcard count
	(define (get-wild-wild-count)
		(get-count (LLOBJ 'wild-wild)))

	; Set the wildcard-wildcard count
	; Return the atom that holds this count.
	(define (set-wild-wild-count CNT)
		(set-count (LLOBJ 'wild-wild) CNT))

	; Methods on this class.
	(lambda (message . args)
		(case message
			((left-wild-count)      (apply get-left-wild-count args))
			((set-left-wild-count)  (apply set-left-wild-count args))
			((right-wild-count)     (apply get-right-wild-count args))
			((set-right-wild-count) (apply set-right-wild-count args))
			((wild-wild-count)      (get-wild-wild-count))
			((set-wild-wild-count)  (apply set-wild-wild-count args))
			(else                   (apply LLOBJ (cons message args))))
	)
)

; ---------------------------------------------------------------------

(define*-public (add-pair-freq-api LLOBJ
    #:optional (ID (LLOBJ 'id)))
"
  add-pair-freq-api LLOBJ ID - Extend LLOBJ with frequency getters.

  Extend the LLOBJ with additional methods to get and set
  the observation frequencies, entropies and mutual infomation.
  Basically, this decorates the class with additional methods
  that get and set these frequencies and entropies in \"standardized\"
  places. Other classes can overload these methods; these just
  provide a reasonable default.

  Here, the LLOBJ is expected to be an object, with methods for
  'item-pair 'make-pair 'left-wildcard and 'right-wildcard on it,
  in the form documented above for the \"low-level API class\".

  The optional ID argument should be #f or a string, used to construct
  the key under which the values are stored.

  The methods are as below.  PAIR is the pair (x,y)

  'pair-freq PAIR    -- return P(x,y)
  'pair-logli PAIR   -- return -log_2 P(x,y)
  'pair-entropy PAIR -- return -P(x,y) log_2 P(x,y)
  'pair-mi PAIR      -- return +P(x,y) log_2 P(x,y) / [P(x,*) P(*,y)]
  'pair-fmi PAIR     -- return +log_2 P(x,y) / [P(x,*) P(*,y)]

  Note the sign convention for the mutual information - it is PLUS log.
  This agrees with both Deniz Yuret and with  Wikipedia!

  In the methods below, ATOM is either the atom x or the atom y.

  'left-wild-freq ATOM   -- return P(*,y) == sum_x P(x,y)
  'left-wild-logli ATOM  -- return -log_2 P(*,y)
  'right-wild-freq ATOM  -- return P(x,*) == sum_y P(x,y)
  'right-wild-logli ATOM -- return -log_2 P(x,*)

  'left-wild-entropy ATOM   -- return h_left(y) = -sum_x p(x,y) log_2 p(x,y)
  'left-wild-fentropy ATOM  -- return H_left(y) = h_left(y) / P(*,y)
  'right-wild-entropy ATOM  -- return h_right(x) = -sum_y p(x,y) log_2 p(x,y)
  'right-wild-fentropy ATOM -- return H_right(x) = h_right(x) / P(x,*)

  Note that H_total = sum_y h_left(y)
                    = sum_x h_right(x)
                    = sum_y P(*,y) H_left(y)
                    = sum_x P(x,*) H_right(x)
  should hold, up to rounding errors.

  For the below, mi(x,y) = -P(x,y) log_2 P(x,y) / [P(x,*) P(*,y)]

  'left-wild-mi ATOM   -- return mi_left(y) = sum_x mi(x,y)
  'left-wild-fmi ATOM  -- return MI_left(y) = mi_left(y) / P(*,y)
  'right-wild-mi ATOM  -- return mi_right(x) = sum_y mi(x,y)
  'right-wild-fmi ATOM -- return MI_right(x) = mi_right(x) / P(x,*)

  Note that MI_total = sum_y mi_left(y)
                     = sum_x mi_right(x)
                     = sum_y P(*,y) MI_left(y)
                     = sum_x P(x,*) MI_right(x)
  should hold, up to rounding errors.

"
	; ----------------------------------------------------
	; Key under which the frequency values are stored.
	(define freq-name
		(if (and ID (LLOBJ 'filters?))
			(string-append "*-FrequencyKey " ID)
			"*-FrequencyKey-*"))

	(define freq-key (PredicateNode freq-name))

	; Return the observed frequency on ATOM
	(define (get-freq ATOM)
		(if (null? ATOM) 0
			(cog-value-ref (cog-value ATOM freq-key) 0)))

	; Return the observed -log_2(frequency) on ATOM
	(define (get-logli ATOM)
		(if (null? ATOM) +inf.0
			(cog-value-ref (cog-value ATOM freq-key) 1)))

	; Return the observed -frequency * log_2(frequency) on ATOM
	(define (get-entropy ATOM)
		(if (null? ATOM) 0
			(cog-value-ref (cog-value ATOM freq-key) 2)))

	; Set both a frequency count, and a -log_2(frequency) on
	; the ATOM.
	(define (set-freq ATOM FREQ)
		; 1.4426950408889634 is 1/0.6931471805599453 is 1/log 2
		(define ln2 (* -1.4426950408889634 (log FREQ)))
		(define ent (* FREQ ln2))
		(cog-set-value! ATOM freq-key (FloatValue FREQ ln2 ent)))

	; ----------------------------------------------------
	; Key under which the entropy values are stored.
	(define entr-name
		(if (and ID (LLOBJ 'filters?))
			(string-append "*-Entropy Key " ID)
			"*-Entropy Key-*"))

	(define entropy-key (PredicateNode entr-name))

	; Return the total entropy on ATOM
	(define (get-total-entropy ATOM)
		(cog-value-ref (cog-value ATOM entropy-key) 0))

	; Return the fractional entropy on ATOM
	(define (get-fractional-entropy ATOM)
		(cog-value-ref (cog-value ATOM entropy-key) 1))

	; Set the entropy value for ATOM.
	(define (set-entropy ATOM ENT FRENT)
		(cog-set-value! ATOM entropy-key (FloatValue ENT FRENT)))

	; ----------------------------------------------------
	; The key under which the MI is stored.
	(define mi-name
		(if (and ID (LLOBJ 'filters?))
			(string-append "*-Mutual Info Key " ID)
			"*-Mutual Info Key-*"))

	(define mi-key (PredicateNode mi-name))

	; Get the (floating-point) mutual information on ATOM.
	(define (get-total-mi ATOM)
		(cog-value-ref (cog-value ATOM mi-key) 0))

	; Get the (floating-point) fractional mutual information on ATOM.
	; This is the Yuret "lexical attraction" value.
	(define (get-fractional-mi ATOM)
		(cog-value-ref (cog-value ATOM mi-key) 1))

	; Set the MI value for ATOM.
	(define (set-mi ATOM MI FMI)
		(cog-set-value! ATOM mi-key (FloatValue MI FMI)))

	; ----------------------------------------------------
	; ----------------------------------------------------
	; Return the observational frequency on PAIR.
	; If the PAIR does not exist (was not oberved) return 0.
	(define (get-pair-freq PAIR)
		(get-freq (LLOBJ 'item-pair PAIR)))

	(define (get-pair-logli PAIR)
		(get-logli (LLOBJ 'item-pair PAIR)))

	(define (get-pair-entropy PAIR)
		(get-entropy (LLOBJ 'item-pair PAIR)))

	; Set the frequency and log-frequency on PAIR
	; Return the atom that holds this count.
	(define (set-pair-freq PAIR FREQ)
		(set-freq (LLOBJ 'make-pair PAIR) FREQ))

	; ----------------------------------------------------

	; Return the MI value on the pair.
	; The MI is defined as
	; + P(x,y) log_2 P(x,y) / P(x,*) P(*,y)
	(define (get-pair-mi PAIR)
		(get-total-mi (LLOBJ 'item-pair PAIR)))

	; Return the fractional MI (lexical atraction) on the pair.
	; + log_2 P(x,y) / P(x,*) P(*,y)
	; It differs from the MI above only by the leading probability.
	(define (get-pair-fmi PAIR)
		(get-fractional-mi (LLOBJ 'item-pair PAIR)))

	(define (set-pair-mi PAIR MI FMI)
		(set-mi (LLOBJ 'item-pair PAIR) MI FMI))

	; ----------------------------------------------------
	; Get the left wildcard frequency
	(define (get-left-wild-freq ITEM)
		(get-freq (LLOBJ 'left-wildcard ITEM)))

	(define (get-left-wild-logli ITEM)
		(get-logli (LLOBJ 'left-wildcard ITEM)))

	; Get the right wildcard frequency
	(define (get-right-wild-freq ITEM)
		(get-freq (LLOBJ 'right-wildcard ITEM)))

	(define (get-right-wild-logli ITEM)
		(get-logli (LLOBJ 'right-wildcard ITEM)))

	; Set the left wildcard frequency.
	; Return the atom that holds this value.
	(define (set-left-wild-freq ITEM FREQ)
		(set-freq (LLOBJ 'left-wildcard ITEM) FREQ))

	; Set the right wildcard frequency.
	; Return the atom that holds this value.
	(define (set-right-wild-freq ITEM FREQ)
		(set-freq (LLOBJ 'right-wildcard ITEM) FREQ))

	; ----------------------------------------------------
	; Get the left wildcard entropy
	; This is defined as
	;   h_left(y) = -sum_x p(x,y) log_2 p(x,y)
	(define (get-left-wild-entropy ITEM)
		(get-total-entropy (LLOBJ 'left-wildcard ITEM)))

	; This is defined as
	;   H_left(y) = h_left(y) / p(*,y)
	(define (get-left-wild-fentropy ITEM)
		(get-fractional-entropy (LLOBJ 'left-wildcard ITEM)))

	; Get the right wildcard entropy
	; This is defined as
	;   h_right(x) = -sum_y p(x,y) log_2 p(x,y)
	(define (get-right-wild-entropy ITEM)
		(get-total-entropy (LLOBJ 'right-wildcard ITEM)))

	; This is defined as
	;   H_left(y) = h_left(y) / p(*,y)
	(define (get-right-wild-fentropy ITEM)
		(get-fractional-entropy (LLOBJ 'right-wildcard ITEM)))

	; Set the left wildcard entropy and fractional entropy.
	; Return the atom that holds this value.
	(define (set-left-wild-entropy ITEM ENT FRENT)
		(set-entropy (LLOBJ 'left-wildcard ITEM) ENT FRENT))

	; Set the right wildcard entropy and fractional entropy.
	; Return the atom that holds this value.
	(define (set-right-wild-entropy ITEM ENT FRENT)
		(set-entropy (LLOBJ 'right-wildcard ITEM) ENT FRENT))

	; ----------------------------------------------------
	; Get the left wildcard mutual information
	(define (get-left-wild-mi ITEM)
		(get-total-mi (LLOBJ 'left-wildcard ITEM)))

	(define (get-left-wild-fmi ITEM)
		(get-fractional-mi (LLOBJ 'left-wildcard ITEM)))

	; Get the right wildcard mutual information
	(define (get-right-wild-mi ITEM)
		(get-total-mi (LLOBJ 'right-wildcard ITEM)))

	(define (get-right-wild-fmi ITEM)
		(get-fractional-mi (LLOBJ 'right-wildcard ITEM)))

	; Set the left wildcard mi and fractional mi.
	; Return the atom that holds this value.
	(define (set-left-wild-mi ITEM MI FRMI)
		(set-mi (LLOBJ 'left-wildcard ITEM) MI FRMI))

	; Set the right wildcard mi and fractional mi.
	; Return the atom that holds this value.
	(define (set-right-wild-mi ITEM MI FRMI)
		(set-mi (LLOBJ 'right-wildcard ITEM) MI FRMI))

	; ----------------------------------------------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((pair-freq)           (apply get-pair-freq args))
			((pair-logli)          (apply get-pair-logli args))
			((pair-entropy)        (apply get-pair-entropy args))
			((pair-mi)             (apply get-pair-mi args))
			((pair-fmi)            (apply get-pair-fmi args))
			((set-pair-freq)       (apply set-pair-freq args))
			((set-pair-mi)         (apply set-pair-mi args))

			((left-wild-freq)      (apply get-left-wild-freq args))
			((left-wild-logli)     (apply get-left-wild-logli args))
			((set-left-wild-freq)  (apply set-left-wild-freq args))

			((right-wild-freq)     (apply get-right-wild-freq args))
			((right-wild-logli)    (apply get-right-wild-logli args))
			((set-right-wild-freq) (apply set-right-wild-freq args))

			((left-wild-entropy)      (apply get-left-wild-entropy args))
			((left-wild-fentropy)     (apply get-left-wild-fentropy args))
			((set-left-wild-entropy)  (apply set-left-wild-entropy args))

			((right-wild-entropy)     (apply get-right-wild-entropy args))
			((right-wild-fentropy)    (apply get-right-wild-fentropy args))
			((set-right-wild-entropy) (apply set-right-wild-entropy args))

			((left-wild-mi)      (apply get-left-wild-mi args))
			((left-wild-fmi)     (apply get-left-wild-fmi args))
			((set-left-wild-mi)  (apply set-left-wild-mi args))

			((right-wild-mi)     (apply get-right-wild-mi args))
			((right-wild-fmi)    (apply get-right-wild-fmi args))
			((set-right-wild-mi) (apply set-right-wild-mi args))

			(else                (apply LLOBJ (cons message args)))))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
