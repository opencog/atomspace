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
; "parametric polymorphism", or sometimes called "traits".
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
; Example low-level API class. It provides a handful of core methods;
; these return atoms on which observation counts and other kinds of
; info are stored as values. Higher-level objects use this object to
; access this information.
;
; Most users will find it easiest to use the `make-evaluation-pair-api`
; to provide the low-level API.  This provides a default, generic pair
; object of the form
;    `EdgeLink PredicateNode "..." ListLink ...`.
; where the PredicateNode identifies what kind of object it is, and the
; ListLink specifies the specific pair.  See `eval-pair.scm` for details.
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
;     ;  (Edge (Predicate "foo") (List (Word "row") (Word "col")))
;     (define (get-pair-type) 'EdgeLink)
;
;     ; Return the atom for a matrix (row,column) pair, if it exists,
;     ; else return nil. In this example, the matrix is defined by an
;     ; EdgeLink holding the ListLink. This atom is where all
;     ; values associated with this matrix are held.  This includes not
;     ; only the count (the number of observations of the pair) but also
;     ; any derived values, such as frequency, mutual information, and
;     ; so on. Users are free to (are encouraged to) use this atom to
;     ; attach additional information and statistics.
;     ;
;     (define (get-pair L-ATOM R-ATOM)
;        (define maybe-list (cog-link 'ListLink L-ATOM R-ATOM))
;        (if (nil? maybe-list) #f
;           (cog-link 'EdgeLink (Predicate "foo") maybe-list)))
;
;     ; Return the atom holding the count, creating it if it does
;     ; not yet exist.  Returns the same structure as the 'get-pair
;     ; method (the get-pair function, above).
;     (define (make-pair L-ATOM R-ATOM)
;        (Edge (Predicate "foo") (List L-ATOM R-ATOM)))
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
;        (EdgeLink (Predicate "foo")
;           (ListLink (AnyNode "left-wild") ITEM)))
;
;     ; Return an atom to which row subtotals can be attached,
;     ; such as, for example, the subtotal `N(x,*)`. Thus, `x`
;     ; denotes a row, and the star is on the right (the star
;     ; ranging over all columns).
;     (define (get-right-wildcard ITEM)
;        (EdgeLink (Predicate "foo")
;           (ListLink ITEM (AnyNode "right-wild"))))
;
;     ; Return an atom to which matrix totals can be attached,
;     ; such as, for example, the total `N(*,*)`. This can be any
;     ; atom, but must be unique to the specific matrix. It's
;     ; convenient to use the same style as the subtotals.
;     (define (get-wild-wild)
;        (EdgeLink (Predicate "foo")
;           (ListLink (AnyNode "left-wild") (AnyNode "right-wild"))))
;
;     ; Retrieve, from storage, the entire matrix, including the
;     ; subtotal and total anchor atoms (the atoms where the marginals
;     ; are stored).  In this example, its enough to get the incoming
;     ; set of (Predicate "foo"), but this need not generally be the case.
;     (define (fetch-all-pairs)
;        (fetch-incoming-by-type (Predicate "foo") 'EdgeLink))
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
;              ((get-pair) get-pair)
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

  The supported methods are:
  'left-basis - Return all items (atoms) that can be used to index
      a row in the matrix.  That is, given a matrix `N(x,y)`, this
      returns the set `{x | (x,y) exists in the atomspace for some y}`.
      All of the elements of this set will be atoms of type
      `(LLOBJ 'left-type)`.  A check is made to verify that `(x,y)` is
      a valid pair, viz. that it is an atom whose type is
      `(LLOBJ 'pair-type)` and that `y` is of type `(LLOBJ 'right-type)`.

  'right-basis - Likewise, but for columns.

  'left-basis-size - the size of the 'left-basis set; i.e. the number
      of unique, distinct atoms in that set.

  'right-basis-size - Likewise.

  'in-left-basis? ROW - Return #t if ROW is an Atom in the left basis;
      else return #f. This predicate is much faster than searching the
      left-basis list directly.

  'in-right-basis? COL - Likewise.

  'left-duals COL - Return the set of rows for which the pair
      `(row, COL)` exists in the atomspace.  That is, return the set
          `{ x | (x,COL) exists in the atomspace }`
      The returned rows will all be of type `(LLOBJ 'left-type)`.
      The input COL atom must be of type `(LLOBJ 'right-type)`.

  'right-duals ROW - Likewise, but returns the columns for `(ROW, *)`.

  'left-stars COL - Return the set of pairs `(row, column)` for
      which the column is `COL`, and the pair exists in the atomspace.
      That is, return the set
         (*, COL) == { (x,COL) | (x,COL) exists in the atomspace }
      The returned pairs will all be of type `(LLOBJ 'pair-type)`,
      and the x's will all be of type `(LLOBJ 'left-type)`. The
      input COL atom must be of type `(LLOBJ 'right-type)`.

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

  Here, the LLOBJ is expected to be the object that defines a pair
  in the AtomSpace. All such objects provide the following methods:

  'name - A one-sentence description of the pair object.
  'id - A one-word id for the object; can be used to build strings.

  'left-type, 'right-type - Returns the types of the Atoms making up
      each side of the pair.
  'pair-type - Returns the type of the Atom holding the pair.

  'get-pair L R - Returns the Atom holding the pair (L,R). The returned
      Atom will be of type 'pair-type. All statistics and information
      about this pair are attached as Values on this Atom.

  'make-pair L R - Create the Atom holding the pair, if it does not
      already exist.

  'left-element P - Return the atom on the left of the pair P.
  'right-element P - Return the atom on the right of the pair P.
      These two together undo what 'make-pair creates.

  'left-wildcard R - Return the marginal atom for the column R.
      The column must be an Atom of type 'right-type. The marginal
      Atom will be of type 'pair-type. The marginal Atom contains
      all information applicable to this column, such as sums over
      counts. (This info is held in Values on this Atom).

  'right-wildcard L - Same as above, but for row L.

  'wild-wild - Same as above, except that this applies matrix-wide.
      This returns the Atom (of type 'pair-type) that holds all
      information applicable to the matrix, as a whole. This includes
      a grand-total count.

  'provides - Return a list of methods that should be used in place of
      the default stars implementation.

  'filters - Used in filtering out certain rows, columns or individual
      entries. Return #f if none.
"
	(let ((l-basis #f)
			(r-basis #f)
			(l-size 0)
			(r-size 0)
			(in-l-basis? #f)
			(in-r-basis? #f)

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

		; Deduce the basis from the collection of pairs.
		; First, get all the valid pairs. Then get the left or right
		; else for those pairs. Deduplicate as we do this.
		(define (do-get-basis LEF)
			(define is-unord (cog-subtype? 'UnorderedLink (LLOBJ 'pair-type)))
			(define basis-set (make-atom-set))
			(if is-unord
				(for-each (lambda (PR)
						(basis-set (LLOBJ 'left-element PR))
						(basis-set (LLOBJ 'right-element PR)))
					(get-all-pairs))
				(if LEF
					(for-each (lambda (PR) (basis-set (LLOBJ 'left-element PR)))
						(get-all-pairs))
					(for-each (lambda (PR) (basis-set (LLOBJ 'right-element PR)))
						(get-all-pairs))))
			(basis-set #f))

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

		(define (is-in-left-basis? ATOM)
			(if (not in-l-basis?)
				(set! in-l-basis? (make-aset-predicate (get-left-basis))))
			(in-l-basis? ATOM))

		(define (is-in-right-basis? ATOM)
			(if (not in-r-basis?)
				(set! in-r-basis? (make-aset-predicate (get-right-basis))))
			(in-r-basis? ATOM))

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

			; XXX FIXME the query can return duplicates.
			; The `remove-duplicate-atoms` will remove the duplicates,
			; but it pays a hefty performance premium. Better if this
			; was done in C++, in the query call.
			(remove-duplicate-atoms rv))

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
		(define (query-all-pairs)
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

		; Make sure that the found pairs are acceptable to LLOBJ
		; Sometimes, pairs don't belong to LLOBJ, if they don't
		; have a specific key on them; we don't have a way of
		; performing a query for the presence of that key. Mostly
		; because we cannot assume that's how the object actually
		; works.
		(define (get-all-pairs)
			(define (valid? PR)
				(LLOBJ 'get-pair
					(LLOBJ 'left-element PR)
					(LLOBJ 'right-element PR)))
			(filter valid? (query-all-pairs)))

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
				(f-in-left-basis?   (overload 'in-left-basis? is-in-left-basis?))
				(f-in-right-basis?  (overload 'in-right-basis? is-in-right-basis?))
				(f-left-stars       (overload 'left-stars get-left-stars))
				(f-right-stars      (overload 'right-stars get-right-stars))
				(f-left-duals       (overload 'left-duals get-left-duals))
				(f-right-duals      (overload 'right-duals get-right-duals))
				(f-get-all-elts     (overload 'get-all-elts get-all-pairs))
			)

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
					((in-left-basis?)   f-in-left-basis?)
					((in-right-basis?)  f-in-right-basis?)
					((left-stars)       f-left-stars)
					((right-stars)      f-right-stars)
					((left-duals)       f-left-duals)
					((right-duals)      f-right-duals)
					((get-all-elts)     f-get-all-elts)

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
					((in-left-basis?)   (apply f-in-left-basis? args))
					((in-right-basis?)  (apply f-in-right-basis? args))
					((left-stars)       (apply f-left-stars args))
					((right-stars)      (apply f-right-stars args))
					((left-duals)       (apply f-left-duals args))
					((right-duals)      (apply f-right-duals args))
					((get-all-elts)     (f-get-all-elts))

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
