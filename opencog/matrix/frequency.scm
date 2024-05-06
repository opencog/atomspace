;
; frequency.scm
;
; Provide traits to get and set the observation frequencies, entropies and
; mutual information.  Given a matrix object, this provides a standardized
; location to hold these quantities.
;
; Copyright (c) 2017 Linas Vepstas
;

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------

(define*-public (add-pair-freq-api LLOBJ
    #:optional (ID (LLOBJ 'id))
    #:key (nothrow #f)
)
"
  add-pair-freq-api LLOBJ ID - Extend LLOBJ with frequency getters.

  Extend the LLOBJ with additional methods to get and set the
  observation frequencies, entropies and mutual information.
  Basically, this decorates the class with additional methods
  that get and set these frequencies and entropies in standardized
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
  This agrees with both Deniz Yuret and with Wikipedia!

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

  For the below, mi(x,y) = P(x,y) log_2 P(x,y) / [P(x,*) P(*,y)]

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
		(if (nil? ATOM) (zero ATOM)
			(let ((val (cog-value ATOM freq-key)))
				(if (nil? val) (zero ATOM) (cog-value-ref val 0)))))

	; Return the observed -log_2(frequency) on ATOM
	(define (get-logli ATOM)
		(if (nil? ATOM) (plus-inf ATOM)
			(let ((val (cog-value ATOM freq-key)))
				(if (nil? val) (plus-inf ATOM) (cog-value-ref val 1)))))

	; Return the observed -frequency * log_2(frequency) on ATOM
	(define (get-entropy ATOM)
		(if (nil? ATOM) (zero ATOM)
			(let ((val (cog-value ATOM freq-key)))
				(if (nil? val) (zero ATOM) (cog-value-ref val 2)))))

	; Set the frequency and -log_2(frequency) on the ATOM.
	; Return the atom that holds this count.
	(define (set-freq ATOM FREQ)
		; 1.4426950408889634 is 1/0.6931471805599453 is 1/log 2
		(define ln2 (* -1.4426950408889634 (log FREQ)))
		; zero log zero is zero, not NaN.  ln2 zero is +inf.0 which is OK.
		(define ent (if (and (real? ln2) (finite? ln2)) (* FREQ ln2) 0))
		(cog-set-value! ATOM freq-key (FloatValue FREQ ln2 ent)))

	; ----------------------------------------------------
	; Key under which the entropy values are stored.
	(define entr-name
		(if (and ID (LLOBJ 'filters?))
			(string-append "*-Entropy Key " ID)
			"*-Entropy Key-*"))

	(define entropy-key (PredicateNode entr-name))

	(define (ezero ATOM) (if nothrow 0
		 (error "No such value! Did you forget to compute entropies?\n\turn `(batch-all-pair-mi LLOBJ)` to compute them." ATOM)))

	(define (eminus-inf ATOM) (if nothrow -inf.0
		 (error "No such value! Did you forget to compute entropies?\n\turn `(batch-all-pair-mi LLOBJ)` to compute them." ATOM)))

	; Return the total entropy on ATOM
	(define (get-total-entropy ATOM)
		(if (nil? ATOM) (ezero ATOM)
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
		(if (nil? ATOM) (eminus-inf ATOM)
			(let ((val (cog-value ATOM mi-key)))
				(if (nil? val) (eminus-inf ATOM) (cog-value-ref val 0)))))

	; Return the fractional MI (lexical attraction) on ATOM.
	; + log_2 P(x,y) / P(x,*) P(*,y)
	; It differs from the MI above only by the leading probability.
	; This is the Yuret "lexical attraction" value.
	(define (get-fractional-mi ATOM)
		(if (nil? ATOM) (eminus-inf ATOM)
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
;
(define-public (make-compute-freq LLOBJ)
"
  make-compute-freq LLOBJ -- compute frequencies and marginals.

  Extend the LLOBJ with additional methods to compute observation
  frequencies (probabilities) and marginal probabilities. The results
  can be cached so that the `add-pair-freq-api` can access them.

  Given a count N(l,r) of observed occurrences for a matrix pair (l,r)
  == (left,right) == (row,column), the frequency aka 'probability' is
  defined as

     P(l,r) = N(l,r) / N(*,*)

  where N(*,*) = sum_l sum_r N(l,r) is the grand-total observation count.
  The marginal probabilities are

      P(l,*)  = N(l,*)  / N(*,*)
      P(*,r)  = N(*,r)  / N(*,*)

  The N(l,*) and N(*,r) are the marginal counts, sometimes called the
  'wild-card counts', and are defined as sums over all observed left and
  right counts.  That is,

      N(l,*) = sum_r N(l,r)
      N(*,r) = sum_l N(l,r)

  Given an object containing the raw counts `N(l,r)`, the count
  marginals are computed by the `add-support-compute` object. Because
  these take considerable CPU time to compute, the resulting values
  are cached, and can be accessed with the `add-support-api` object.

  This object, `make-compute-freq`, assumes that the count marginals
  have already been computed; i.e. that LLOBJ has valid left and right
  wild-card counts on it.

  Before using this class, the 'init-freq method must be called,
  and it must be called *after* a valid wild-wild count is available.

  Methods on this object:

  'compute-pair-freq PAIR -- Given the PAIR (x,y), compute the pair
       frequency P(x,y) = N(x,y) / N(*,*). This is the frequency with
       which the pair (x,y) is observed. Return the frequency, or zero,
       if the pair was never observed.
  'compute-left-freq COL  -- Compute the left-side wild-card frequency
        for column COL. This is P(*,y) = N(*,y) / N(*,*) = sum_x P(x,y)
        with `y` being COL.
  'compute-right-freq ROW -- As above, but for the indicated ROW.

  'cache-pair-freq PAIR   -- As above, and the value is cached where the
        `add-pair-freq-api` can access it later.
  'cache-left-freq COL    -- As above.
  'cache-right-freq ROW   -- As above.

  'cache-all-left-freqs  -- Compute and cache all left marginals.
  'cache-all-right-freqs -- Compute and cache all right marginals.
  'cache-all-pair-freqs  -- Compute and cache all pair frequencies.
  'cache-all             -- Do all three of the above.
"
	; We need 'left-basis, provided by add-pair-stars
	; We need 'wild-wild-count, provided by add-support-api
	; We need 'set-left-wild-freq, provided by add-pair-freq-api
	(define supobj (add-support-api LLOBJ))
	(define frqobj (add-pair-freq-api LLOBJ))
	(define wldobj (add-pair-stars LLOBJ))
	(define tot-cnt #f)

	(define (init)
		(set! tot-cnt (supobj `wild-wild-count)))

	; Compute the pair frequency P(x,y) = N(x,y) / N(*,*)  This is
	; the frequency with which the pair (x,y) is observed. Return
	; the frequency, or zero, if the pair was never observed.
	(define (compute-pair-freq PAIR)
		(/ (LLOBJ 'get-count PAIR) tot-cnt))

	; Compute the left-side wild-card frequency. This is the ratio
	; P(*,y) = N(*,y) / N(*,*) = sum_x P(x,y)
	(define (compute-left-freq ITEM)
		(/ (supobj 'left-count ITEM) tot-cnt))
	(define (compute-right-freq ITEM)
		(/ (supobj 'right-count ITEM) tot-cnt))

	; Compute and cache the pair frequency.
	; This returns the atom holding the cached count, thus
	; making it convenient to persist (store) this cache in
	; the database. It returns nil if the count was zero.
	(define (cache-pair-freq PAIR)
		(define freq (compute-pair-freq PAIR))
		(frqobj 'set-pair-freq PAIR freq)
		(if (< 0 freq) PAIR #f))

	; Compute and cache the left-side wild-card frequency.
	; This is unconditional - even if the frequency is zero.
	(define (cache-left-freq ITEM)
		(frqobj 'set-left-wild-freq ITEM (compute-left-freq ITEM)))

	(define (cache-right-freq ITEM)
		(frqobj 'set-right-wild-freq ITEM (compute-right-freq ITEM)))

	; Compute and cache all of the pair frequencies.
	; This computes P(x,y) for all (x,y)
	; This returns a count of the pairs.
	; Also caches the total dimensions of the matrix.
	(define (cache-all-pair-freqs)
		(define cnt 0)
		; The outer-loop.
		(define (right-loop left-item)
			(for-each
				(lambda (pr)
					(cache-pair-freq pr)
					(set! cnt (+ cnt 1)))
				(wldobj 'right-stars left-item)))

		; par-for-each fails massively here in guile-2.9.2
		(for-each right-loop (wldobj 'left-basis))

		; Return the total.
		cnt)

	; Compute and cache all of the left-side frequencies.
	; This computes P(*,y) for all y.
	; par-for-each fails here in guile-2.9.2
	(define (cache-all-left-freqs)
		(for-each cache-left-freq (wldobj 'right-basis)))
	(define (cache-all-right-freqs)
		(for-each cache-right-freq (wldobj 'left-basis)))

	(define (cache-all)
		(init)
		(cache-all-left-freqs)
		(cache-all-right-freqs)
		(cache-all-pair-freqs))

	; Methods on this class.
	(lambda (message . args)
		(case message
			((init-freq)             (init))

			((compute-pair-freq)     (apply compute-pair-freq args))
			((compute-left-freq)     (apply compute-left-freq args))
			((compute-right-freq)    (apply compute-right-freq args))

			((cache-pair-freq)       (apply cache-pair-freq args))
			((cache-left-freq)       (apply cache-left-freq args))
			((cache-right-freq)      (apply cache-right-freq args))

			((cache-all-pair-freqs)  (cache-all-pair-freqs))
			((cache-all-left-freqs)  (cache-all-left-freqs))
			((cache-all-right-freqs) (cache-all-right-freqs))
			((cache-all)             (cache-all))

			(else (apply LLOBJ       (cons message args))))
	)
)

; ---------------------------------------------------------------------
