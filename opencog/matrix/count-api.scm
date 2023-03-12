;
; count-api.scm
;
; Define object-oriented class APIs that handle counting.
;
; Copyright (c) 2017, 2022 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; One of the most common tasks for which the matrices are employed is for
; counting. Rather than requiring each object to implement it's own
; counting API, this file provides some convenience wrappers to get, set
; and increment counts. These can be applied to any objects.
;
; More precisely, we are generally interested in pairs (x,y) of atoms
; (that is, where x and y are atoms), and we have some sort of count
; N(x,y) of how often that particular pair was observed. The objects here
; can be used to define where N(x,y) is stored, and how its updated.
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
; Three API's are provided:
; -- add-count-api, which provides basic counting.
; -- add-storage-count, same as above, but fetches & updates storage.
; -- add-marginal-count, same as above, but updates marginal counts.
; The last two can be combined to safely update marginal counts in storage.
; The storage API should be "below" the marginal API; the marginal API
; will use it to perform the storage.
;
; The API's support both single (scalar) numbers and counts that are
; vectors. For example, it can be useful to maintain a simple count and
; a weighted count at the same time.
;
; ---------------------------------------------------------------------

(use-modules (ice-9 threads))
(use-modules (srfi srfi-1))
(use-modules (opencog))

; ---------------------------------------------------------------------

(define-public (add-count-api LLOBJ)
"
  add-count-api LLOBJ - Extend LLOBJ with default methods to get, set
  and increment the counts on pairs.

  The provided methods are defaults: they are exposed and used only
  if LLOBJ does not already provide them. Otherwise, the methods on
  LLOBJ take precedence.

  The provided methods are all thread-safe.  Counts in attached storage
  are NOT updated; nor are Atoms fetch from storage prior to update.
  Use the `add-storage-count` API to get storage updates.

  In particular, the 'inc-count method is thread-safe: it will perform
  an atomic increment: an atomic read-modify-write.

  Counts may be single numeric scalars (floats) or they may be a vector
  of floats. Vectors can be useful when working with weighted counts.

  The provided methods are:

  'pair-count L R - Returns the total observed count on the pair (L,R)
      L must be an Atom of type 'left-type on the base object LLOBJ,
      and likewise for R. Returns zero if such a pair does not exist.
      For vector counts, #f ir returned if the pair does not exist.

  'pair-set L R N - Sets the total observed count to N on the pair (L,R).
      Creates the pair, if it does not yet exist. N may be a single
      number (a scalar) or a vector.

  'pair-inc L R N - Increments the total observed count by N on the
      pair (L,R).  Creates the pair, if it does not yet exist. N may
      be a single number (a scalar) or a vector.

   The above three methods are built on the three below. These do the
   same as above, but take the pair Atom directly, instead of the two
   index Atoms. If LLOBJ already provides the below, then that will
   will be used to implement the above.

  'get-count P - Returns the total observed count on the pair P.
      The P atom should be one of the atoms returned by the LLOBJ
      'get-pair method.

  'set-count P N - Set the total observed count to N on the pair P.

  'inc-count P N - Perform an atomic increment of the count on P by N.
       The increment is atomic, meaning that it is thread-safe against
       racing threads.

  The next method provides an atomic transfer of counts from one
  location to another. It is built on top of the above methods.

  'move-count ACC DONOR FRAC - Move a fraction FRAC of the count from
       DONOR to ACC. The move is atomic, in that no counts are lost in
       the case of racing threads performing other count updates.
       ACC and DONOR should be two pairs in this matrix.
       FRAC should be a numeric fraction, between 0.0 and 1.0.

   The final three methods provide a simplfied API to store values
   in non-standard, custom locations. If LLOBJ provides these methods,
   they will be used to build all of the other methods.

   'count-key - Return the key at which the the count is stored. If the
       base class LLOBJ provides this method, then this key will be used
       for all access. If the base class does not provide this, then the
       default of `(PredicateNode \"*-TruthValueKey-*\")` is used.

   'count-type - Return the type of the Value holding the count. If the
       base class LLOBJ provides this method, then this type will be used
       when first creating a Value to hold the count. If the base class
       does not provide this, then the default of `CountTruthValue` will
       be used. The count-type should be a subtype of type `FloatValue`,
       as this is the only type capable of holding numbers.

   'count-ref - Return the offset into the vector of the Value holding
       the scalar count. If the base class LLOBJ provides this method,
       then this offset will be used when setting or incrementing the
       scalar count.  If the base class does not provide this, then the
       default of 2 will be used. This default is the location of the
       count field on `CountTruthValue`'s and is thus backwards-compat
       with older code.

       If 'count-ref is provided and returns #f, then all counts will
       be treated as vectors, so that gets, sets and increments all
       operate on vectors.
"
	; By default, the count is stored as a CountTruthValue.
	; That means that it is on the TruthValue Key, and is the
	; third number (the first two being strength and confidence.)
	; These defaults are used if and only if the base object does
	; not provide these.
	(define (count-type) 'CountTruthValue)
	(define (count-key) (PredicateNode "*-TruthValueKey-*"))
	(define (count-ref) 2)

	; See if the base object provides the type, key and ref.
	(define (get-loc symbol default)
		(define fp (LLOBJ 'provides symbol))
		(if fp (fp) default))

	; Get the type, key and ref from the base, if it is provided.
	; Otherwise, use the defaults.
	(define cnt-type (get-loc 'count-type (count-type)))
	(define cnt-key (get-loc 'count-key (count-key)))
	(define cnt-ref (get-loc 'count-ref (count-ref)))

	; Avoid insanity.
	(define chkt (if (not (cog-subtype? 'FloatValue cnt-type))
		(throw 'wrong-type-arg 'add-count-api
			"Count type must be a FloatValue; got ~A!" (list cnt-type))))

	; -------------------------------------------------------
	; The three basic routines to access scalar counts.

	; Return the observed count for the pair PAIR.
	(define (get-scalar-count PAIR)
		(define cv (cog-value PAIR cnt-key))
		(if cv (cog-value-ref cv cnt-ref) 0))

	; Explicitly set location to value
	(define (set-scalar-count PAIR CNT)
		(if (not (equal? cnt-type (cog-value-type PAIR cnt-key)))
			(cog-set-value! PAIR cnt-key
				(cog-new-value cnt-type (make-list (+ cnt-ref 1) 0))))
		(cog-set-value-ref! PAIR cnt-key CNT cnt-ref))

	; Increment location. Unlike cog-set-value-ref!, this will
	; automatically create the FloatValue (or CountTruthValue).
	(define (inc-scalar-count PAIR CNT)
		(cog-inc-value! PAIR cnt-key CNT cnt-ref))

	; -------------------------------------------------------
	; The three basic routines to access vector counts.
	; This are "trivial", and provide little utility, other than
	; providing a uniform counting interface. Not clear if we should
	; have bothered...

	; Return the observed count for the pair PAIR.
	(define (get-vector-count PAIR) (cog-value PAIR cnt-key))

	; Explicitly set location to value.
	(define (set-vector-count PAIR VEC)
		(cog-set-value! PAIR cnt-key VEC))

	; Increment vector
	(define (inc-vector-count PAIR VEC)
		(cog-update-value! PAIR cnt-key VEC))

	; -------------------------------------------------------
	; If LLOBJ does not provides the symbol, return that.
	; If LLOBJ does not provide it, then return the scalar default
	; if cnt-ref is defined and is non-negative. Else use the vector.
	(define (verload symbol scalar-default vector-default)
		(define fp (LLOBJ 'provides symbol))
		(if fp fp
			(if (and cnt-ref (<= 0 cnt-ref))
				scalar-default vector-default)))

	; Use the functions defined above, but only if the low-level
	; object does not already provide them. If it does, use what
	; is provided. We need these three, to finish the rest of the
	; implementation, below.
	(define f-get-count     (verload 'get-count get-scalar-count get-vector-count))
	(define f-set-count     (verload 'set-count set-scalar-count set-vector-count))
	(define f-inc-count     (verload 'inc-count inc-scalar-count inc-vector-count))

	; -------------------------------------------------------

	; Return the observed count for the pair (L-ATOM, R-ATOM), if it
	; exists, else return zero.
	(define (pair-count L-ATOM R-ATOM)
		(define stats-atom (LLOBJ 'get-pair L-ATOM R-ATOM))
		(if (nil? stats-atom) 0 (f-get-count stats-atom)))

	(define (pair-set L-ATOM R-ATOM CNT)
		(f-set-count (LLOBJ 'make-pair L-ATOM R-ATOM) CNT))

	(define (pair-inc L-ATOM R-ATOM CNT)
		(f-inc-count (LLOBJ 'make-pair L-ATOM R-ATOM) CNT))

	; Accumulate a fraction FRAC of the count from DONOR into ACC.
	(define (move-count ACCUM DONOR FRAC)
		; Return #t if the count is effectively zero.
		; Use an epsilon for rounding errors.
		(define (is-zero? cnt) (< cnt 1.0e-10))

		; The counts on the accumulator and the pair to merge.
		(define donor-cnt (f-get-count DONOR))
		(define frac-cnt (* FRAC donor-cnt))

		; If there is nothing to transfer over, do nothing.
		(when (not (is-zero? frac-cnt))
			(f-inc-count ACCUM frac-cnt)
			(f-inc-count DONOR (- frac-cnt)))

		; Return how much was transferred over.
		frac-cnt)

	;-------------------------------------------

	(define (help)
		(format #t
			(string-append
"This is the `add-count-api` object applied to the \"~A\"\n"
"object.  It provides generic counting support methods for the base\n"
"object. This is a core utility, widely used to simplify counting. For\n"
"more information, say `,d add-count-api` or `,describe add-count-api`\n"
"at the guile prompt, or just use the 'describe method on this object.\n"
"You can also get at the base object with the 'base method: e.g.\n"
"`((obj 'base) 'help)`.\n"
)
			(LLOBJ 'id)))

	(define (describe)
		(display (procedure-property add-count-api 'documentation)))

	; -------------------------------------------------------
	; Provide default methods, but only if the low-level object
	; does not already provide them.
	(define (overload symbol default)
		(define fp (LLOBJ 'provides symbol))
		(if fp fp default))

	(define f-count-type    (overload 'count-type count-type))
	(define f-count-key     (overload 'count-key count-key))
	(define f-count-ref     (overload 'count-ref count-ref))
	(define f-pair-count    (overload 'pair-count pair-count))
	(define f-pair-set      (overload 'pair-set pair-set))
	(define f-pair-inc      (overload 'pair-inc pair-inc))
	(define f-move-count    (overload 'move-count move-count))

	;-------------------------------------------
	; Explain what is provided.
	(define (provides meth)
		(case meth
			((count-type)    f-count-type)
			((count-key)     f-count-key)
			((count-ref)     f-count-ref)
			((pair-count)    f-pair-count)
			((pair-set)      f-pair-set)
			((pair-inc)      f-pair-inc)
			((get-count)     f-get-count)
			((set-count)     f-set-count)
			((inc-count)     f-inc-count)
			((move-count)    f-move-count)

			(else            (LLOBJ 'provides meth))))

	;-------------------------------------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((count-type)       (f-count-type))
			((count-key)        (f-count-key))
			((count-ref)        (f-count-ref))
			((pair-count)       (apply f-pair-count args))
			((pair-set)         (apply f-pair-set args))
			((pair-inc)         (apply f-pair-inc args))
			((get-count)        (apply f-get-count args))
			((set-count)        (apply f-set-count args))
			((inc-count)        (apply f-inc-count args))
			((move-count)       (apply f-move-count args))

			((provides)         (apply provides args))
			((help)             (help))
			((describe)         (describe))
			((obj)              "add-count-api")
			((base)             LLOBJ)
			(else               (apply LLOBJ (cons message args))))
	))

; ---------------------------------------------------------------------

(define-public (add-storage-count LLOBJ)
"
  add-storage-count LLOBJ - Extend LLOBJ with methods to get, set and
  increment the counts on pairs, fetching them from storage, or updating
  storage, as necessary.

  All updates will be thread-safe. After update, the new count will be
  written to storage. If there's no existing count, it will be fetched
  from storage.

  This is sufficient to maintain single-user, multi-threaded storage.
  It is NOT safe to use with multi-user storage, because the counts
  are fetched only once!

  See `add-count-api` for a description of the provided methods.
"
	(define count-obj (add-count-api LLOBJ))

	; Get the type and key from the base.
	(define cnt-type (count-obj 'count-type))
	(define cnt-key (count-obj 'count-key))

	(define mtx (make-mutex))

	; Return the observed count for the pair PAIR.
	; If the pair has nothing at the storage key, fetch it.
	; If the pair has the wrong type, e.g. SimpleTV instead of CountTV
	; then fetch it. Be surgical w/ the fetch. Get only what we need.
	;
	; This uses a mutex to minimize, but not avoid a race window
	; involving fetch-and-add. Users that try to fetch non-existant
	; values, and then increment them will need to use locks to avoid
	; fetch-after-increment.
	(define (get-count PAIR)
		(when (not (equal? cnt-type (cog-value-type PAIR cnt-key)))
			(lock-mutex mtx)
			(if (not (equal? cnt-type (cog-value-type PAIR cnt-key)))
				(fetch-value PAIR cnt-key))
			(unlock-mutex mtx))
		(count-obj 'get-count PAIR))

	(define (set-count PAIR CNT)
		(count-obj 'set-count PAIR CNT)
		(store-value PAIR cnt-key))

	; Fully thread-safe fetch-and-increment.
	(define (inc-count PAIR CNT)
		(if (not (equal? cnt-type (cog-value-type PAIR cnt-key)))
			(begin
				(lock-mutex mtx)
				(if (not (equal? cnt-type (cog-value-type PAIR cnt-key)))
					(fetch-value PAIR cnt-key))
				(count-obj 'inc-count PAIR CNT)
				(unlock-mutex mtx))
			(count-obj 'inc-count PAIR CNT))
		(store-value PAIR cnt-key))

	; Return the observed count for the pair (L-ATOM, R-ATOM).
	; Be careful to not create tha pair, if it doesn't exist.
	; The logic below:
	; If the pair exists, then just fetch the count.
	; If the pair does not exist,
	;    -- create a temp atomspace,
	;    -- create the pair in that temp space
	;    -- fetch value from storage.
	;    -- if the value was found
	;         -- fetch value in mainspace
	;    -- else if not found, defer to LLOBJ about what to return.
	;
	(define (pair-count L-ATOM R-ATOM)
		(define test-pair (LLOBJ 'get-pair L-ATOM R-ATOM))
		(if (not (nil? test-pair))
			(get-count test-pair)
			(let* ((base (cog-push-atomspace))
					(PAIR (LLOBJ 'make-pair L-ATOM R-ATOM))
					(junk (fetch-value PAIR cnt-key))
					(have-it (equal? cnt-type (cog-value-type PAIR cnt-key))))
				(cog-pop-atomspace)
				(if have-it
					(get-count (LLOBJ 'make-pair L-ATOM R-ATOM))
					(LLOBJ 'pair-count L-ATOM R-ATOM)))))

	(define (pair-set L-ATOM R-ATOM CNT)
		(set-count (LLOBJ 'make-pair L-ATOM R-ATOM) CNT))

	(define (pair-inc L-ATOM R-ATOM CNT)
		(inc-count (LLOBJ 'make-pair L-ATOM R-ATOM) CNT))

	; Accumulate a fraction FRAC of the count from DONOR into ACC.
	(define (move-count ACCUM DONOR FRAC)
		; Return #t if the count is effectively zero.
		; Use an epsilon for rounding errors.
		(define (is-zero? cnt) (< cnt 1.0e-10))

		; The counts on the accumulator and the pair to merge.
		(define donor-cnt (get-count DONOR))
		(define frac-cnt (* FRAC donor-cnt))

		; If there is nothing to transfer over, do nothing.
		(when (not (is-zero? frac-cnt))
			(inc-count ACCUM frac-cnt)
			(inc-count DONOR (- frac-cnt)))

		; Return how much was transferred over.
		frac-cnt)

	;-------------------------------------------

	(define (help)
		(format #t
			(string-append
"This is the `add-storage-count` object applied to the \"~A\"\n"
"object.  It provides the same API as `add-count-api`, but updates\n"
"counts in storage. For more information, say `,d add-storage-count`\n"
"at the guile prompt, or just use the 'describe method on this object.\n"
"You can also get at the base object with the 'base method: e.g.\n"
"`((obj 'base) 'help)`.\n"
)
			(LLOBJ 'id)))

	(define (describe)
		(display (procedure-property add-storage-count 'documentation)))

	;-------------------------------------------
	; Explain what is provided.
	(define (provides meth)
		(case meth
			((pair-count)    pair-count)
			((pair-set)      pair-set)
			((pair-inc)      pair-inc)
			((get-count)     get-count)
			((set-count)     set-count)
			((inc-count)     inc-count)
			((move-count)    move-count)

			(else            (LLOBJ 'provides meth))))

	;-------------------------------------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((pair-count)       (apply pair-count args))
			((pair-set)         (apply pair-set args))
			((pair-inc)         (apply pair-inc args))
			((get-count)        (apply get-count args))
			((set-count)        (apply set-count args))
			((inc-count)        (apply inc-count args))
			((move-count)       (apply move-count args))

			((provides)         (apply provides args))
			((help)             (help))
			((describe)         (describe))
			((obj)              "add-storage-count")
			((base)             LLOBJ)
			(else               (apply LLOBJ (cons message args))))
	))

; ---------------------------------------------------------------------

(define-public (add-marginal-count LLOBJ)
"
  add-marginal-count LLOBJ - Extend LLOBJ with methods that update
  marginal counts whenever the pair-count is updated.

  The goal of this object is to keep the marginal counts in sync with
  the pair counts, so that the marginal counts are always valid. That
  is, when the count on a pair (L,R) is updated, then the wild-card
  counts (L,*) and (*,R) and (*,*) are updated.  In other words, if an
  increment delta is added to N(L,R), then this same increment is added
  to right-marginal count N(L,*), the left-marginal count N(*,R) and
  the total pair count N(*,*).

  The increments are thread-safe against multiple updating threads, in
  that the marginals are eventually updated in a consistent manner.
  However, the update of the marginals is not atomic with respect to the
  update of the pair count. For example, there is a small timing window
  in which it is possible to read the new N(L,R) while still reading the
  old N(L,*).

  To write the updates to storage, the LLOBJ should be set to the
  `add-storage-count` object; its methods will be used to store the
  updated pair count as well as the marginal counts.

  The provided methods are:

  'pair-inc L R N - Increments the total observed count by N on the
      pair (L,R).  Creates the pair, if it does not yet exist. In
      addition, the count on (L,*) and (*,R) and (*,*) are updated
      as well.

  'inc-count P N - Perform an atomic increment of the count on P by N.
       The pair P is decomposed into (L,R) and the counts are updated on
       (L,*) and (*,R) and (*,*) as well.

  The two setter methods are also pprovided, but their use is discouraged.
  This is because, in order to keep the marginals in sync, the old pair
  counts must be fetched first, and a delta taken against the new count.
  This delta is needed to adjust the marginals correctly.  In other words,
  the setters below are converted into the incrementers above. So there
  is no benefit to using the below.

  'pair-set L R N - Sets the total observed count to N on the pair (L,R).
      Creates the pair, if it does not yet exist. Updates the marginals
      appropriately.

  'set-count P N - Set the total observed count to N on the pair P.
      Updates the marginals appropriately.
"
	(define count-obj (add-count-api LLOBJ))
	(define wild-wild (LLOBJ 'wild-wild))

	(define (inc-count PAIR CNT)
		(define L-ATOM (LLOBJ 'left-element))
		(define R-ATOM (LLOBJ 'right-element))
		(count-obj 'inc-count (LLOBJ 'left-wildcard R-ATOM) CNT)
		(count-obj 'inc-count (LLOBJ 'right-wildcard L-ATOM) CNT)
		(count-obj 'inc-count wild-wild CNT)

		; Do this last; its the return value.
		(count-obj 'inc-count PAIR CNT))

	; Increment the counts, and update the marginals.
	(define (pair-inc L-ATOM R-ATOM CNT)
		(count-obj 'inc-count (LLOBJ 'left-wildcard R-ATOM) CNT)
		(count-obj 'inc-count (LLOBJ 'right-wildcard L-ATOM) CNT)
		(count-obj 'inc-count wild-wild CNT)

		; Do this last; its the return value.
		(count-obj 'inc-count (LLOBJ 'make-pair L-ATOM R-ATOM) CNT))

	; FIXME: Maybe we should be throwing an exception here?
	; Trying the just "set" the count while maintaining marginals
	; is bad form, and shouldn't really be done. We work around this
	; by computing a delta, and using that to adjuct the marginal
	; counts.
	(define (set-count PAIR CNT)
		(inc-count PAIR (- CNT (count-obj 'get-count PAIR))))

	(define (pair-set L-ATOM R-ATOM CNT)
		(pair-inc L-ATOM R-ATOM
			(- CNT (count-obj 'pair-count L-ATOM R-ATOM))))

	;-------------------------------------------

	(define (help)
		(format #t
			(string-append
"This is the `add-marginal-count` object applied to the \"~A\"\n"
"object.  It provides the same API as `add-count-api`, but updates\n"
"marginal counts whenever pair counts are changed. For more information,\n"
"say `,d add-marginal-count` at the guile prompt, or just use the\n"
"'describe method on this object. You can also get at the base object\n"
"with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
			(LLOBJ 'id)))

	(define (describe)
		(display (procedure-property add-marginal-count 'documentation)))

	;-------------------------------------------
	; Explain what is provided.
	(define (provides meth)
		(case meth
			((pair-set)      pair-set)
			((pair-inc)      pair-inc)
			((set-count)     set-count)
			((inc-count)     inc-count)

			(else            (LLOBJ 'provides meth))))

	;-------------------------------------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((pair-set)         (apply pair-set args))
			((pair-inc)         (apply pair-inc args))
			((set-count)        (apply set-count args))
			((inc-count)        (apply inc-count args))

			((provides)         (apply provides args))
			((help)             (help))
			((describe)         (describe))
			((obj)              "add-marginal-count")
			((base)             LLOBJ)
			(else               (apply LLOBJ (cons message args))))
	))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
