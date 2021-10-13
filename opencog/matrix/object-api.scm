;
; object-api.scm
;
; Define object-oriented class API's for correlation matrices of atoms.
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
; the pair (x,y) occurred,
;
; For all of these pairs (x,y), we typically need to get the count
; N(x,y), the partial sums N(x,*) = sum_y N(x,y), and likewise N(*,y)
; and N(*,*).   We need to compute frequencies of observations, such
; as p(x,y) = N(x,y)/N(*,*).  We also need to compute entropies and
; mutual information, which can be inferred from these frequencies.
; We also can compute cosine-similarity and other metrics of similarity,
; derived solely from the observed frequency counts.
;
; All of these formulas are independent of the actual objects in the
; pairs.  Thus, it is useful to separate the various algorithms from
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
; The system here could have been implemented with srfi-9 "Defining
; record types" but using srfi-9 seems to offer little or no advantage.
; Using srfi-57 "Records" does not seem to offer an advantage.
;
; XXX FIXME ... method overloading is screwy and sometimes backwards.
; For example, the filter object overloads a bunch of methods, and
; it overloads in the "normal" direction: when calling a method, the
; method on the filter should be called. However, the stars object
; works in the "backwards" direction: it provides default methods,
; but only if the underlying object does not already provide them.
; A proper OO wrapper would allow both kinds of overloading.
;
; ---------------------------------------------------------------------
;
; There are several API's in this file. The lowest-level ones are listed first.
;
; Example low-level API class. It provides a handful of core methods;
; these return atoms on which observation counts are stored as values.
; Higher-level objects use this object to fetch counts, store them
; into the database, or to compute and return various statistics.
;
; Most users will find it easiest to use the `make-evaluation-pair-api`
; to provide the low-level API.  It is ideal, when counts are stored
; in the form of `EvaluationLink PredicateNode "..." ListLink ...`.
; See `eval-pair.scm` for details.
;
; The `add-pair-freq-api` class, below, is a typical user of this
; class; it provides getters and setters for the frequencies.
;
; See `make-evaluation-pair-api` and `make-any-link-api` for working
; examples.
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
;     ; Complex types are allowed. The above can also be written as
;     (define (get-left-type) (Type 'WordNode))
;     (define (get-left-type) (TypeChoice (Type 'WordNode) (Type 'WordClassNode)))
;
;     ; Return the atom-type of the matrix columns, i.e. the type of
;     ; the right side of the (row, column) pairs.
;     (define (get-right-type) 'WordNode)
;
;     ; Return the type of the link that holds the pair.  In this
;     ; example, each pair will be held in the form
;     ;  (Evaluation (Predicate "foo") (List (Word "row") (Word "col")))
;     (define (get-pair-type) 'EvaluationLink)
;
;     ; Return the atom for a matrix (row,column) pair, if it exists,
;     ; else return nil. In this example, the matrix is defined by an
;     ; EvaluationLink holding the ListLink. This atom is where all
;     ; values associated with this matrix are held.  This includes not
;     ; only the count (the number of observations of the pair) but also
;     ; any derived values, such as frequency, mutual information, and
;     ; so on. Users are free to (are encouraged to) use this atom to
;     ; attach additional information and statistics.
;     ;
;     ; Optional; if not provided, the stars object will provide a default
;     (define (get-pair L-ATOM R-ATOM)
;        (define maybe-list (cog-link 'ListLink L-ATOM R-ATOM))
;        (if (nil? maybe-list) #f
;           (cog-link 'EvaluationLink (Predicate "foo") maybe-list)))
;
;     ; Return the observed count for the pair PAIR.
;     ; Optional; if not provided, the stars object will provide a default
;     (define (get-count PAIR)
;        (cog-value-ref (cog-value PAIR (Predicate "counter")) 42))
;
;     ; Return the observed count for the pair (L-ATOM, R-ATOM), if it
;     ; exists, else return zero.
;     ; Optional; if not provided, the stars object will provide a default
;     (define (get-pair-count L-ATOM R-ATOM)
;        (define stats-atom (get-pair L-ATOM R-ATOM))
;        (if (nil? stats-atom) #f (get-count stats-atom)))
;
;     ; Return the atom holding the count, creating it if it does
;     ; not yet exist.  Returns the same structure as the 'get-pair
;     ; method (the get-pair function, above).
;     ; Optional; if not provided, the stars object will provide a default
;     (define (make-pair L-ATOM R-ATOM)
;        (Evaluation (Predicate "foo") (List L-ATOM R-ATOM)))
;
;     ; Return the atom that forms the left, resp. right side of
;     ; the pair. These undo what 'make-pair does.
;     (define (get-pair-left PAIR) (gadr PAIR))
;     (define (get-pair-rigt PAIR) (gddr PAIR))
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
;     ; subtotal and total anchor atoms (the atoms where the marginals
;     ; are stored).  In this example, its enough to get the incoming
;     ; set of (Predicate "foo"), but this need not generally be the case.
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
;              ((name) (lambda () "A Kind of Demonstration Object"))
;              ((id)   (lambda () "demo"))
;              ((left-type) get-left-type)
;              ((right-type) get-right-type)
;              ((pair-type) get-pair-type)
;              ((pair-count) get-pair-count)
;              ((get-pair) get-pair)
;              ((get-count) get-count)
;              ((make-pair) make-pair)
;              ((left-element) get-pair-left)
;              ((right-element) get-pair-right)
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
(use-modules (ice-9 atomic))  ; for atomic-box
(use-modules (ice-9 threads)) ; for mutex locks
(use-modules (opencog) (opencog exec))

; ---------------------------------------------------------------------

(define-public (add-pair-stars LLOBJ)
"
  add-pair-stars LLOBJ - Extend LLOBJ with row and column access
  methods (aka wildcard methods). This is a core utility, widely
  used to simplify iteration over the rows and columns of LLOBJ.

  This also provides default implementations for the `get-pair`,
  `make-pair`, `left-element`, `right-element`, `pair-count`,
  `get-count`, `set-count` and `move-count` methods, in case the
  LLOBJ does not provide them.

  The supported methods are:
  'left-basis - Return all items (atoms) that can be used to index
      a row in the matrix.  That is, given a matrix `N(x,y)`, this
      returns the set `{x | (x,y) exists in the atomspace for some y}`.
      All of the elements of this set will be atoms of type
      `(LLOBJ 'left-type)`.  A check is made to verify that `(x,y)` is
      a valid pair, viz. that it is an atom whose type is
      `(LLOBJ 'pair-type)` and that `y` is of type `(LLOBJ 'right-type)`.
      This only verifies that such pairs exist in the atomspace; it
      does NOT verify that they have a nonzero count!

  'right-basis - Likewise, but for columns.

  'left-basis-size - the size of the 'left-basis set; i.e. the number
      of unique, distinct atoms in that set.

  'right-basis-size - Likewise.

  'left-duals COL - Return the set of rows for which the pair
      `(row, COL)` exists in the atomspace.  That is, return the set
          `{ x | (x,COL) exists in the atomspace }`
      The returned rows will all be of type `(LLOBJ 'left-type)`.
      The input COL atom must be of type `(LLOBJ 'right-type)`.
      This does NOT verify that these pairs have a non-zero count.

  'right-duals ROW - Likewise, but returns the columns for `(ROW, *)`.

  'left-stars COL - Return the set of pairs `(row, column)` for
      which the column is `COL`, and the pair exists in the atomspace.
      That is, return the set
         (*, COL) == { (x,COL) | (x,COL) exists in the atomspace }
      The returned pairs will all be of type `(LLOBJ 'pair-type)`,
      and the x's will all be of type `(LLOBJ 'left-type)`. The
      input COL atom must be of type `(LLOBJ 'right-type)`.  This does
      NOT verify that these pairs have a non-zero count.

  'right-stars ROW - Likewise, but returns the set `(ROW, *)`.

  'get-all-elts - Return a list of all elements in the matrix.
      Caution: this may be very large, and thus take a long time to
      compute.  The result is not cached, so if you call this a second
      time, this list will be recomputed from scratch!

  Note that for `(define STARS (add-pair-stars LLOBJ))`
  the list returned by
    `(map (lambda (COL) (LLOBJ 'get-pair ROW COL)) (STARS 'right-duals ROW))`
  should be equal to the list
    `(STARS 'right-stars ROW)`
  and so this offers two different ways of iterating over the same
  list of pairs.

  This also provides default implementations for the following methods,
  in case the base LLOBJ object did not provide them:

  'pair-count L R - Returns the total observed count on the pair (L,R)
      L must be an Atom of type 'left-type and likewise for R.

  'get-pair L R - Returns the Atom holding the pair (L,R). The returned
      Atom will be of type 'pair-type. All statistics and information
      about this pair are attached as Values on this Atom.

  'get-count P - Returns the total observed count on the pair P, where
      P is the Atom returned by 'get-pair.

  'make-pair L R - Create the Atom holding the pair, if it does not
      already exist.

  'left-element P - Return the atom on the left of the pair P.
  'right-element P - Return the atom on the right of the pair P.
      These two together undo what 'make-pair creates.


  Here, the LLOBJ is expected to be the object that defines a pair
  in the AtomSpace. All such objects provide the following methods:

  'name - A one-sentence description of the pair object.
  'id - A one-word id for the object; can be used to build strings.

  'left-type, 'right-type - Returns the types of the Atoms making up
      each side of the pair.
  'pair-type - Returns the type of the Atom holding the pair.

  'left-wildcard R - Return the marginal atom for the column R.
      The column must be an Atom of type 'right-type. The marginal
      Atom will be of type 'pair-type. The marginal Atom contains
      all information applicable to this column, such as sums over
      counts. (This info is held in Values on this Atom).

  'right-wildcard L - Same as above, but for row L.

  'wild-wild - Same as above, except that this applies matrix-wide.
      This returns the Atom (of type 'pair-type) that holds all
      information applicable to the matrix, as a whole. Ths includes
      a grand-total count.

  'fetch-pairs - Fetch all pairs from an open database.

  'provides - Return a list of methods that should be used in place of
      the default stars implementation.

  'filters - Used in filtering out certain rows, columns or individual
      entries. Return #f if none.
"
	(define pair-type (LLOBJ 'pair-type))
	(define pair-type-str
		(cond
			((symbol? pair-type) (symbol->string pair-type))
			((and (cog-atom? pair-type) (equal? (cog-type pair-type) 'TypeNode))
				(cog-name pair-type))
			(else #f)))

	; Basic defaults for base class methods. If the base class provides
	; these, then the base-classes methods will be used.
	(define (get-pair L R)
		(cog-link pair-type-str L R))

	(define (make-pair L R)
		(cog-new-link pair-type-str L R))

	(define (left-element PR) (gar PR))
	(define (right-element PR) (gdr PR))

	; Get the observational count on ATOM.
	(define (get-count ATOM) (cog-count ATOM))
	(define (set-count ATOM CNT)
		(cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

	; Return the raw observational count on PAIR. If the counter for
	; PAIR does not exist (was not observed), then return 0.
	(define (pair-count L R)
		(define pr (LLOBJ 'get-pair L R))
		(if (nil? pr) 0 (LLOBJ 'get-count pr)))

	; Accumulate a fraction FRAC of the count from DONOR into ACC.
	; ACC and DONOR should be two pairs in this matrix.
	; FRAC should be a numeric fraction, between 0.0 and 1.0.
	; This is not thread-safe!
	(define (move-count ACCUM DONOR FRAC)
		; Return #t if the count is effectively zero.
		; Use an epsilon for rounding errors.
		(define (is-zero? cnt) (< cnt 1.0e-10))

		; The counts on the accumulator and the pair to merge.
		(define donor-cnt (LLOBJ 'get-count DONOR))
		(define frac-cnt (* FRAC donor-cnt))
		(define rem-cnt (- donor-cnt frac-cnt))

		; If there is nothing to transfer over, do nothing.
		(when (not (is-zero? frac-cnt))

			; The accumulated count
			(LLOBJ 'set-count ACCUM (+ frac-cnt (LLOBJ 'get-count ACCUM)))

			; Update the count on the donor pair.
			(LLOBJ 'set-count DONOR rem-cnt)
		)

		; Return how much was transfered over.
		frac-cnt
	)

	(let ((l-basis #f)
			(r-basis #f)
			(l-size 0)
			(r-size 0)

			; Caches for the left and right stars
			(star-l-hit (make-atomic-box '()))
			(star-l-miss (make-atomic-box '()))
			(star-r-hit (make-atomic-box '()))
			(star-r-miss (make-atomic-box '()))

			; Caches for the left and right duals
			(dual-l-hit (make-atomic-box '()))
			(dual-l-miss (make-atomic-box '()))
			(dual-r-hit (make-atomic-box '()))
			(dual-r-miss (make-atomic-box '()))

#! ============ Alternate variant, not currently used. See below.
			; Temporary atomspaces, fluid style.
			(fluasp (make-fluid))

			; Temporary atomspaces, non-fluid style.
			(mtx (make-mutex))
			(aspace (cog-new-atomspace (cog-atomspace)))
=============== !#
		)

		; LLOBJ can return either a symbol or an Atom. If it's a
		; symbol, its a primitive type name.
		(define lt (LLOBJ 'left-type))
		(define left-type (if (cog-atom? lt) lt (Type lt)))

		(define rt (LLOBJ 'right-type))
		(define right-type (if (cog-atom? rt) rt (Type rt)))

		; Perform a query to find all atoms that might appear on
		; the left, or the right of a pair.  Return a list of them.
		(define (do-get-basis LEF)
			(define uleft (uniquely-named-variable))
			(define uright (uniquely-named-variable))
			(define term (LLOBJ 'make-pair uleft uright))
			(define queue (cog-execute! (Query
				(VariableList
					(TypedVariable uleft left-type)
					(TypedVariable uright right-type))
				term (if LEF uleft uright))))
			(cog-extract-recursive! uleft)
			(cog-extract-recursive! uright)
			(cog-value->list queue))

		; Return a list of all of the atoms that might ever appear on
		; the left-hand-side of a pair.  This is the set of all possible
		; items x from the pair (x,y) for any y.
		;
		(define (get-left-basis)
			(if (not l-basis) (set! l-basis (do-get-basis #t)))
			l-basis)

		(define (get-right-basis)
			(if (not r-basis) (set! r-basis (do-get-basis #f)))
			r-basis)

		(define (get-left-size)
			(if (eq? 0 l-size) (set! l-size (length (get-left-basis))))
			l-size)

		(define (get-right-size)
			(if (eq? 0 r-size) (set! r-size (length (get-right-basis))))
			r-size)

		; -------------------------------------------------------
		; Return default, only if LLOBJ does not provide symbol
		(define (overload symbol default)
			(define fp (LLOBJ 'provides symbol))
			(if fp fp default))

		; Define default patterns, that, when executed, return the stars.
		; The LLOBJ can provide custom versions of this.
		(define (default-left-star-pat ITEM VAR)
			(let ((term (LLOBJ 'make-pair VAR ITEM)))
				(Query (TypedVariable VAR left-type)
					term term)))

		(define (default-right-star-pat ITEM VAR)
			(let ((term (LLOBJ 'make-pair ITEM VAR)))
				(Query (TypedVariable VAR right-type)
					term term)))

		(define f-left-star-var
			(overload 'left-star-variable uniquely-named-variable))
		(define f-right-star-var
			(overload 'right-star-variable uniquely-named-variable))

		(define f-left-star-pat
			(overload 'left-star-pattern default-left-star-pat))
		(define f-right-star-pat
			(overload 'right-star-pattern default-right-star-pat))

		; -------------------------------------------------------
		; Same as above, but for the duals, not the stars.
		; The pattern is almost the same, except that this time,
		; we return basis elements.
		;
		; Define default patterns, that, when executed, return the duals.
		; The LLOBJ can provide custom versions of this.
		(define (default-left-dual-pat ITEM VAR)
			(let ((term (LLOBJ 'make-pair VAR ITEM)))
				(Meet (TypedVariable VAR left-type) term)))

		(define (default-right-dual-pat ITEM VAR)
			(let ((term (LLOBJ 'make-pair ITEM VAR)))
				(Meet (TypedVariable VAR right-type) term)))

		(define f-left-dual-var
			(overload 'left-dual-variable uniquely-named-variable))
		(define f-right-dual-var
			(overload 'right-dual-variable uniquely-named-variable))

		(define f-left-dual-pat
			(overload 'left-dual-pattern default-left-dual-pat))
		(define f-right-dual-pat
			(overload 'right-dual-pattern default-right-dual-pat))

		; -------------------------------------------------------
		;
		; Handy wrapper. Run the MeetLink/QueryLink and return
		; the results. Recursively delete the var, so that we don't
		; have a pile-up of QueryLinks in the atomspace.
		(define (run-query FUNC ITEM VARGEN)
			(define var (VARGEN))
			(define rv (cog-value->list (cog-execute! (FUNC ITEM var))))
			(if (cog-atom? var) (cog-extract-recursive! var))
			rv)

		; -------------------------------------------------------
		;
		; Cache the most recent two values.  This should offer a
		; significant performance enhancement for computing cosines
		; of vectors, as usually, one of the cosine-pair is constant
		; (and will be a cache-hit) while the other always misses.
		(define (do-run-query A-HIT A-MISS GETTER ITEM)
			(define hit (atomic-box-ref A-HIT))
			(if (null? hit)
				; If hit is empty, then set it, and return the value
				(let ((stars (GETTER ITEM)))
					(atomic-box-set! A-HIT (list ITEM stars))
					stars)
				; If hit is not empty, and has the right key, then
				; return the value. If hit has the wrong value, try
				; again with miss.
				(if (equal? ITEM (car hit))
					(cadr hit)
					(let ((miss (atomic-box-ref A-MISS)))
						; If we can match miss, then promote miss to a hit.
						; Else compute a value and cache it.
						(if (and (not (null? miss)) (equal? ITEM (car miss)))
							(begin
								(atomic-box-set! A-HIT miss)
								(atomic-box-set! A-MISS '())
								(cadr miss))
							(let ((stars (GETTER ITEM)))
								(atomic-box-set! A-MISS (list ITEM stars))
								stars))))))

		; Apply caching to the stars
		(define (get-left-stars ITEM)
			(do-run-query star-l-hit star-l-miss
				(lambda (itm)
					(run-query f-left-star-pat itm f-left-star-var)) ITEM))

		; Same as above, but on the right.
		(define (get-right-stars ITEM)
			(do-run-query star-r-hit star-r-miss
				(lambda (itm)
					(run-query f-right-star-pat itm f-right-star-var)) ITEM))

		; Apply caching to the duals
		(define (get-left-duals ITEM)
			(do-run-query dual-l-hit dual-l-miss
				(lambda (itm)
					(run-query f-left-dual-pat itm f-left-dual-var)) ITEM))

		; Same as above, but on the right.
		(define (get-right-duals ITEM)
			(do-run-query dual-r-hit dual-r-miss
				(lambda (itm)
					(run-query f-right-dual-pat itm f-right-dual-var)) ITEM))

		; Invalidate the caches. This is needed, when atoms get deleted.
		(define (clobber)
			(atomic-box-set! star-l-hit '())
			(atomic-box-set! star-l-miss '())
			(atomic-box-set! star-r-hit '())
			(atomic-box-set! star-r-miss '())

			(atomic-box-set! dual-l-hit '())
			(atomic-box-set! dual-l-miss '())
			(atomic-box-set! dual-r-hit '())
			(atomic-box-set! dual-r-miss '())

			; The basis may have changed, too.
			(set! l-basis #f)
			(set! r-basis #f)
			(set! l-size 0)
			(set! r-size 0)

			; Pass it on to the LLOBJ, too.
			(if (LLOBJ 'provides 'clobber) (LLOBJ 'clobber))
		)

		;-------------------------------------------
		; Perform a query to find all (non-marginal) matrix entries
		; in the matrix. Return a list of them.
		(define (get-all-pairs)
			(define uleft (uniquely-named-variable))
			(define uright (uniquely-named-variable))
			(define term (LLOBJ 'make-pair uleft uright))
			(define queue (cog-execute! (Query
				(VariableList
					(TypedVariable uleft left-type)
					(TypedVariable uright right-type))
				term term)))
			(cog-extract-recursive! uleft)
			(cog-extract-recursive! uright)
			(cog-value->list queue))

		;-------------------------------------------

		(define (help)
			(format #t
				(string-append
"This is the `add-pair-stars` object applied to the \"~A\"\n"
"object.  It provides row and column access methods (aka wildcard\n"
"methods). This is a core utility, widely used to simplify iteration\n"
"over the rows and columns of the base object. For more information, say\n"
"`,d add-pair-stars` or `,describe add-pair-stars` at the guile prompt,\n"
"or just use the 'describe method on this object. You can also get at\n"
"the base object with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
				(LLOBJ 'id)))

		(define (describe)
			(display (procedure-property add-pair-stars 'documentation)))

		;-------------------------------------------

		; Provide default methods, but only if the low-level object
		; does not already provide them. In practice, this is used in
		; two different ways: One is by the fold-api which overloads
		; the stars methods to work differently.  The other is to
		; allow an underlying object to provide cached basis, as these
		; can sometimes take an obscenely long time to compute.
		(let ((f-left-basis       (overload 'left-basis get-left-basis))
				(f-right-basis      (overload 'right-basis get-right-basis))
				(f-left-basis-size  (overload 'left-basis-size get-left-size))
				(f-right-basis-size (overload 'right-basis-size get-right-size))
				(f-left-stars       (overload 'left-stars get-left-stars))
				(f-right-stars      (overload 'right-stars get-right-stars))
				(f-left-duals       (overload 'left-duals get-left-duals))
				(f-right-duals      (overload 'right-duals get-right-duals))
				(f-get-all-elts     (overload 'get-all-elts get-all-pairs))

				(f-get-pair         (overload 'get-pair get-pair))
				(f-make-pair        (overload 'make-pair make-pair))
				(f-left-element     (overload 'left-element left-element))
				(f-right-element    (overload 'right-element right-element))
				(f-pair-count       (overload 'pair-count pair-count))
				(f-get-count        (overload 'get-count get-count))
				(f-set-count        (overload 'set-count set-count))
				(f-move-count       (overload 'move-count move-count)))

			;-------------------------------------------
			; Explain what it is that I provide. The point here is that
			; computing the left and right basis can be very expensive,
			; and so if it has already been computed, that should be used,
			; by deferring to the object that holds those caches. We do
			; by advertising that .. we hold caches.
			(define (provides meth)
				(case meth
					((left-basis)       f-left-basis)
					((right-basis)      f-right-basis)
					((left-basis-size)  f-left-basis-size)
					((right-basis-size) f-right-basis-size)
					((left-stars)       f-left-stars)
					((right-stars)      f-right-stars)
					((left-duals)       f-left-duals)
					((right-duals)      f-right-duals)
					((get-all-elts)     f-get-all-elts)

					((get-pair)         f-get-pair)
					((make-pair)        f-make-pair)
					((left-element)     f-left-element)
					((right-element)    f-right-element)
					((pair-count)       f-pair-count)
					((get-count)        f-get-count)
					((set-count)        f-set-count)
					((move-count)       f-move-count)

					((clobber)          clobber)
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
					((left-duals)       (apply f-left-duals args))
					((right-duals)      (apply f-right-duals args))
					((get-all-elts)     (f-get-all-elts))

					((get-pair)         (apply f-get-pair args))
					((make-pair)        (apply f-make-pair args))
					((left-element)     (apply f-left-element args))
					((right-element)    (apply f-right-element args))
					((pair-count)       (apply f-pair-count args))
					((get-count)        (apply f-get-count args))
					((set-count)        (apply f-set-count args))
					((move-count)       (apply f-move-count args))

					((clobber)          (clobber))
					((provides)         (apply provides args))
					((help)             (help))
					((describe)         (describe))
					((obj)              "add-pair-stars")
					((base)             LLOBJ)
					(else               (apply LLOBJ (cons message args))))
			))))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------

(define*-public (add-pair-freq-api LLOBJ
    #:optional (ID (LLOBJ 'id))
    #:key (nothrow #f)
)
"
  add-pair-freq-api LLOBJ ID - Extend LLOBJ with frequency getters.

  Extend the LLOBJ with additional methods to get and set
  the observation frequencies, entropies and mutual information.
  Basically, this decorates the class with additional methods
  that get and set these frequencies and entropies in \"standardized\"
  places. Other classes can overload these methods; these just
  provide a reasonable default.

  Here, the LLOBJ is expected to be an object, with methods for
  'get-pair 'make-pair 'left-wildcard and 'right-wildcard on it,
  in the form documented above for the \"low-level API class\".

  The optional ID argument should be #f or a string, used to construct
  the key under which the values are stored.

  The optional #:nothrow argument should be set to #t to avoid throwing
  errors for missing values. If this is not set, then any missing value
  will cause an error to be thrown.

  The dataset needs to be populated with valid data, before the getters
  can actually return anything. (The getters do NOT compute the
  requested valued \"on the fly\".)  The needed values can be generated
  with the `make-compute-freq` and `make-batch-mi` classes.  It is
  probably most convenient to use `batch-all-pair-mi` to compute
  everything at once, and save to to the database, all in one go.

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

	(define (zero ATOM) (if nothrow 0
		(error "No such value! Did you forget to compute frequencies?\n\tRun `((make-compute-freq LLOBJ) 'cache-all)` to compute them." ATOM)))

	(define (plus-inf ATOM) (if nothrow +inf.0
		(error "No such value! Did you forget to compute frequencies?\n\tRun `((make-compute-freq LLOBJ) 'cache-all)` to compute them." ATOM)))

	; Return the observational frequency on ATOM.
	; If the ATOM does not exist (or was not observed) return 0.
	(define (get-freq ATOM)
		(if (null? ATOM) (zero ATOM)
			(let ((val (cog-value ATOM freq-key)))
				(if (nil? val) (zero ATOM) (cog-value-ref val 0)))))

	; Return the observed -log_2(frequency) on ATOM
	(define (get-logli ATOM)
		(if (null? ATOM) (plus-inf ATOM)
			(let ((val (cog-value ATOM freq-key)))
				(if (nil? val) (plus-inf ATOM) (cog-value-ref val 1)))))

	; Return the observed -frequency * log_2(frequency) on ATOM
	(define (get-entropy ATOM)
		(if (null? ATOM) (zero ATOM)
			(let ((val (cog-value ATOM freq-key)))
				(if (nil? val) (zero ATOM) (cog-value-ref val 2)))))

	; Set the frequency and -log_2(frequency) on the ATOM.
	; Return the atom that holds this count.
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

	(define (ezero ATOM) (if nothrow 0
		 (error "No such value! Did you forget to compute entropies?\n\tRun `(batch-all-pair-mi LLOBJ)` to compute them." ATOM)))

	(define (eminus-inf ATOM) (if nothrow -inf.0
		 (error "No such value! Did you forget to compute entropies?\n\tRun `(batch-all-pair-mi LLOBJ)` to compute them." ATOM)))

	; Return the total entropy on ATOM
	(define (get-total-entropy ATOM)
		(if (null? ATOM) (ezero ATOM)
			(let ((val (cog-value ATOM entropy-key)))
				(if (nil? val) (ezero ATOM) (cog-value-ref val 0)))))

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

	; Return the MI value on ATOM.
	; The MI is defined as
	; + P(x,y) log_2 P(x,y) / P(x,*) P(*,y)
	(define (get-total-mi ATOM)
		(if (null? ATOM) (eminus-inf ATOM)
			(let ((val (cog-value ATOM mi-key)))
				(if (nil? val) (eminus-inf ATOM) (cog-value-ref val 0)))))

	; Return the fractional MI (lexical attraction) on ATOM.
	; + log_2 P(x,y) / P(x,*) P(*,y)
	; It differs from the MI above only by the leading probability.
	; This is the Yuret "lexical attraction" value.
	(define (get-fractional-mi ATOM)
		(if (null? ATOM) (eminus-inf ATOM)
			(let ((val (cog-value ATOM mi-key)))
				(if (nil? val) (eminus-inf ATOM) (cog-value-ref val 1)))))

	; Set the MI value for ATOM.
	(define (set-mi ATOM MI FMI)
		(cog-set-value! ATOM mi-key (FloatValue MI FMI)))

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

	;-------------------------------------------

	(define (help)
		(format #t
			(string-append
"This is the `add-pair-pair-freq` object applied to the \"~A\"\n"
"object.  It provides access to frequency, entropy and mutual information\n"
"values attached to pairs (to matrix elements). It assumes that these have\n"
"been previously computed; this object only fetches the values from well-\n"
"known locations in the AtomSpace.\n"
"\n"
"For more information, say `,d add-pair-freq-api` at the guile prompt,\n"
"or just use the 'describe method on this object. You can also get at\n"
"the base object with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
			(LLOBJ 'id)))

	(define (describe)
		(display (procedure-property add-pair-freq-api 'documentation)))

	; ----------------------------------------------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((pair-freq)           (apply get-freq args))
			((pair-logli)          (apply get-logli args))
			((pair-entropy)        (apply get-entropy args))
			((pair-mi)             (apply get-total-mi args))
			((pair-fmi)            (apply get-fractional-mi args))
			((set-pair-freq)       (apply set-freq args))
			((set-pair-mi)         (apply set-mi args))

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

			((help)              (help))
			((describe)          (describe))
			((obj)               "add-pair-freq-api")
			((base)              LLOBJ)

			(else                (apply LLOBJ (cons message args)))))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
