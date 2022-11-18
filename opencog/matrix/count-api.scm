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
; -- add-storage-count, same as above, but fectches & updates storage.
; -- add-marginal-count, same as above, but updates marginal counts.
; The last two can be combined to safely update marginal counts in storage.
; The storage API should be "below" the marginal API; the marginal API
; will use it to perform the storage.
;
; ---------------------------------------------------------------------

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

  The provided methods are:

  'pair-count L R - Returns the total observed count on the pair (L,R)
      L must be an Atom of type 'left-type on the base object LLOBJ,
      and likewise for R. Returns zero if such a pair does not exist.

  'pair-set L R N - Sets the total observed count to N on the pair (L,R).
      Creates the pair, if it does not yet exist.

  'pair-inc L R N - Increments the total observed count by N on the
      pair (L,R).  Creates the pair, if it does not yet exist.

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
       default of `(PredicateNode "*-TruthValueKey-*")` is used.

   'count-type - Return the type of the Value holding the count. If the
       base class LLOBJ provides this method, then this type will be used
       when first creating a Value to hold the count. If the base class
       does not provide this, then the default of `CountTruthValue` will
       be used. The count-type should be a subtype of type `FloatValue`,
       as this is the only type capable of holding numbers.

   'count-ref - Return the offset into the vector of the Value holding
       the count. If the base class LLOBJ provides this method, then
       this offset will be used when setting or incrementing the count.
       If the base class does not provide this, then the default of 2
       will be used. This default is the location of the count field on
       `CountTruthValue`'s and is thus backwards-compat with older code.
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
		(if fp fp default))

	; Get the type, key and ref from the base, if it is provided.
	; Otherwise, use the defaults.
	(define cnt-type (get-loc 'count-type (count-type)))
	(define cnt-key (get-loc 'count-key (count-key)))
	(define cnt-ref (get-loc 'count-ref (count-ref)))

	; Avoid insanity.
	(define chkt (if (not (cog-subtype? 'FloatValue cnt-key))
		(throw 'bad-type 'add-count-api "Count type must be a FloatValue!")))

	; -------------------------------------------------------
	; The three basic routines to access counts.

	; Return the observed count for the pair PAIR.
	(define (get-count PAIR)
		(cog-value-ref PAIR cnt-key cnt-ref))

	; Explicitly set location to value
	(define (set-count PAIR CNT)
		(if (not (equal? cnt-type (cog-value-type PAIR cnt-key)))
			(cog-set-value! PAIR (cog-new-value cnt-type
				(make-list (+ cnt-ref 1) 0))))
		(cog-set-value-ref! PAIR cnt-key CNT cnt-ref))

	; Increment location. Unlike cog-set-value-ref!, this will
	; automatically create the FloatValue (or CountTruthValue).
	(define (inc-count PAIR CNT)
		(cog-inc-value! PAIR cnt-key CNT cnt-ref))

	; -------------------------------------------------------
	; Return default, only if LLOBJ does not provide symbol
	(define (overload symbol default)
		(define fp (LLOBJ 'provides symbol))
		(if fp fp default))

	; Use the functions defined above, but only if the low-level
	; object does not already provide them. If it does, use what
	; is provided. We need these three, to finish the rest of the
	; implementation, below.
	(define f-get-count     (overload 'get-count get-count))
	(define f-set-count     (overload 'set-count set-count))
	(define f-inc-count     (overload 'inc-count inc-count))

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

		; Return how much was transfered over.
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
  storage, as neccessary.

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

	; Return the observed count for the pair PAIR.
	; If the pair has nothing at the storage key, fetch it.
	; If the pair has the wrong type, e.g. SimpleTV instead of CountTV
	; then fetch it. Be surgical w/ the fetch. Get only what we need.
	(define (get-count PAIR)
		(if (not (equal? cnt-type (cog-value-type PAIR cnt-key)))
			(fetch-value PAIR cnt-key))
		(count-obj 'get-count PAIR))

	(define (set-count PAIR CNT)
		(count-obj 'set-count PAIR CNT)
		(store-value PAIR cnt-key))

	(define (inc-count PAIR CNT)
		(count-obj 'inc-count PAIR CNT)
		(store-value PAIR cnt-key))

	; Return the observed count for the pair (L-ATOM, R-ATOM).
	; If it does not exist, make it.
	(define (pair-count L-ATOM R-ATOM)
	  (get-count (LLOBJ 'make-pair L-ATOM R-ATOM)))

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

		; Return how much was transfered over.
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

  All updates are thread-safe.

  See `add-count-api` for a description of the provided methods.
  The provided methods are:

  'pair-set L R N - Sets the total observed count to N on the pair (L,R).
      Creates the pair, if it does not yet exist.

  'pair-inc L R N - Increments the total observed count by N on the
      pair (L,R).  Creates the pair, if it does not yet exist.

  'set-count P N - Set the total observed count to N on the pair P.

  'inc-count P N - Perform an atomic increment of the count on P by N.
       The increment is atomic, meaning that it is thread-safe against
       racing threads.

"
	(define count-obj (add-count-api LLOBJ))

	; Get the type and key from the base.
	(define cnt-type (count-obj 'count-type))
	(define cnt-key (count-obj 'count-key))

	(define (set-count PAIR CNT)
		(count-obj 'set-count PAIR CNT)
		(store-value PAIR cnt-key))

	(define (inc-count PAIR CNT)
		(count-obj 'inc-count PAIR CNT)
		(store-value PAIR cnt-key))

	(define (pair-set L-ATOM R-ATOM CNT)
	  (set-count (LLOBJ 'make-pair L-ATOM R-ATOM) CNT))

	(define (pair-inc L-ATOM R-ATOM CNT)
	  (inc-count (LLOBJ 'make-pair L-ATOM R-ATOM) CNT))

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
