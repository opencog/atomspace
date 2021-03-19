;
; compute-mi.scm
;
; Compute the mutual information of pairs of items.
;
; Copyright (c) 2013, 2014, 2017, 2018 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The scripts below compute the mutual information held in pairs
; of "items".  The "items" can be any atoms, arranged in ordered pairs,
; usually by a ListLink. For example,
;
;     ListLink
;          SomeAtom "left-side"
;          OtherKinfOfAtom "right-hand-part"
;
; It is presumed that the atomspace already contains such pairs, with
; counts attached to each. The prototypical example is a word-pair,
; connected with an "ANY" link-grammar link:
;
;   EvaluationLink
;      LinkGrammarRelationshipNode "ANY"
;      ListLink
;         WordNode "some-word"
;         WordNode "other-word"
;
; In the general case, access to this structure is provided by methods
; on the "low-level API". These include:
;   'left-type and 'right-type, both of which should return 'WordNode
;         for the above.
;   'get-pair, which should return the EvaluationLink, given the
;        ListLink
;   'left-wildcard and 'right-wildcard, indicating where the partial
;        sums, such as N(x,*) and N(*,y) should be stored.
;
; Let N(wl,wr) denote the number of times that the pair (wl, wr) has
; actually been observed; that is, N("some-word", "other-word") for the
; example above.  Properly speaking, this count is conditioned on the
; LinkGrammarRelationshipNode "ANY", so the correct notation would be
; N(rel, wl, wr) with `rel` the relationship.  In what follows, the
; relationship is always assumed to be the same, and is thus dropped.
; (the relationship is provided through the GET-PAIR function).
;
; The mutual information for a pair is defined as follows:  Given
; two items, wl and wr, define three probabilities:
;
;    P(wl,wr) = N(wl,wr) / N(*,*)
;    P(wl,*)  = N(wl,*)  / N(*,*)
;    P(*,wr)  = N(*,wr)  / N(*,*)
;
; The N(*,*), N(wl,*) and  N(*,wr) are wild-card counts, and are defined
; to be sums over all observed left and right counts.  That is,
;
;    N(wl,*) = Sum_wr N(wl,wr)
;    N(*,wr) = Sum_wl N(wl,wr)
;    N(*,*) = Sum_wl Sum_wr N(wl,wr)
;
; Given an object containing the raw counts N(wl,wr), these sums are
; computed by the `add-support-compute` object. Because these take
; considerable CPU time to compute, the resulting values are cached,
; and can be obtained with the `add-support-api` object. (This is
; typical throughout the code: there are pairs of objects, one which
; computes marginals, and another that accesses the cached values.)
;
; For example, for word-pair counts, the wild-card sums are stored
; with the atoms
;
;   EvaluationLink
;      LinkGrammarRelationshipNode "ANY"
;      ListLink
;         AnyNode "left-word"
;         WordNode "bird"
;
;   EvaluationLink
;      LinkGrammarRelationshipNode "ANY"
;      ListLink
;         WordNode "word"
;         AnyNode "right-word"
;
;   EvaluationLink
;      LinkGrammarRelationshipNode "ANY"
;      ListLink
;         AnyNode "left-word"
;         AnyNode "right-word"
;
; Here, AnyNode plays the role of *.  Thus, N(*,*) is shorthand for the
; last of these triples.
;
; After they've been computed, the values for N(*,y) and N(x,*) can be
; fetched with the 'left-count and 'right-count methods on the
; support-api object.  The value for N(*,*) can be gotten with the
; 'wild-wild-count method. More correctly, there are two of these
; totals, which should differ only by rounding errors: they differ in
; the order in which the sums are performed.
;
; The fractional mutual information for the pair (x,y) is defined with
; a plus sign, as in Deniz Yuret's thesis (1998, page 40):
;
;     MI(x,y) = + log_2 [ p(x,y) /  p(x,*) p(*,y) ]
;
; This is computed by the script batch-all-pair-mi below. The value is
; attached at the location provided by the 'set-pair-mi method on the
; object.  It can later be retrieved with the corresponding 'pair-mi
; method.
;
; The main utility wrapper not only computes all of these, but also
; stores them in the database, from which they can be retrieved at some
; future date.  This helps avoid repeating the calculation, which can
; take many hours for large datasets (tens of millions of pairs).
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (ice-9 atomic))
(use-modules (ice-9 optargs))  ; Needed for define*-public
(use-modules (ice-9 threads))
(use-modules (opencog))
(use-modules (opencog persist))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; A progress report utility.
; The wraps the FUNC function, and prints a progress report MSG
; every WHEN calls to FUNC.
; FUNC should be the function to be called, taking one argument.
; MSG should be a string of the form
;    "Did ~A of ~A in ~A seconds (~A items/sec)\n"
; WHEN should be how often to print (modulo)
; TOTAL should be the total number of items to process.

(define (make-progress-rpt FUNC WHEN TOTAL MSG)
	(let ((func FUNC)
			(when WHEN)
			(total TOTAL)
			(msg MSG)
			(cnt 0)
			(start-time 0))
		(lambda (item)
			; back-date to avoid divide-by-zero
			(if (eqv? 0 cnt) (set! start-time (- (current-time) 0.00001)))
			(func item)
			(set! cnt (+ 1 cnt))
			(if (eqv? 0 (modulo cnt when))
				(let* ((elapsed (- (current-time) start-time))
						(ilapsed (inexact->exact (round elapsed)))
						(rate (/ (exact->inexact when) elapsed))
						(irate (inexact->exact (round rate)))
					)
					(format #t msg cnt total ilapsed irate)
					(set! start-time (current-time))))))
)

; ---------------------------------------------------------------------
;
(define-public (make-compute-freq LLOBJ)
"
  make-compute-freq LLOBJ

  Extend the LLOBJ with additional methods to compute observation
  frequencies and entropies for pairs, including partial-sum entropies
  (mutual information) for the left and right side of each pair.
  This will also cache the results of these computations in a
  standardized location.

  The LLOBJ must have valid left and right wild-card counts on it.
  These need to have been previously computed, before methods on
  this class are called.

  Before using this class, the 'init-freq method must be called,
  and it must be called *after* a valid wild-wild count is available.
"
	; We need 'left-basis, provided by add-pair-stars
	; We need 'wild-wild-count, provided by add-support-api
	; We need 'set-left-wild-freq, provided by add-pair-freq-api
	(let ((llobj LLOBJ)
			(supobj (add-support-api LLOBJ))
			(frqobj (add-pair-freq-api LLOBJ))
			(wldobj (add-pair-stars LLOBJ))
			(tot-cnt #f))

		(define (init)
			(set! tot-cnt (supobj `wild-wild-count)))

		; Compute the pair frequency P(x,y) = N(x,y) / N(*,*)  This is
		; the frequency with which the pair (x,y) is observed. Return
		; the frequency, or zero, if the pair was never observed.
		(define (compute-pair-freq PAIR)
			(/ (llobj 'get-count PAIR) tot-cnt))

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
			(if (< 0 freq)
				(frqobj 'set-pair-freq PAIR freq)
				'()))

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

				(else (apply llobj       (cons message args))))
		))
)

; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to compute the mutual
; information of all pairs; each pair is then tagged with the resulting
; MI. (This is the "cache" -- the resulting MI is "cached" with the
; atom).
;
; The LLOBJ object must have valid pair-frequencies on it, accessible
; by the standard frequency API. These need to have been pre-computed,
; before using this object.
;
; The MI computations are done as a batch, looping over all pairs.

(define (make-batch-mi LLOBJ)

	; We need 'left-basis, provided by add-pair-stars
	; We need 'pair-freq, provided by add-pair-freq-api
	;      Don't throw, in case of zero counts on a pair.
	; We need 'set-pair-mi, provided by add-pair-freq-api
	; We need 'right-count, provided by add-support-api
	(let ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ))
			(supobj (add-support-api LLOBJ))
			(frqobj (add-pair-freq-api LLOBJ #:nothrow #t)))

		; Loop over all pairs, computing the MI for each. The loop
		; is actually two nested loops, with a loop over the
		; left-basis on the outside, and over right-stars for
		; the inner loop.  The CALLBACK is called once per outer
		; loop, and is passed a list of atoms that hold the MI value,
		; obtained in the inner loop.
		(define (compute-n-cache-pair-mi CALLBACK)
			(define lefties (star-obj 'left-basis))
			(define nlefties (length lefties))

			; progress stats
			(define cnt-pairs (make-atomic-box 0))
			(define cnt-lefties (make-atomic-box 0))

			; Atomic increment of counter.
			(define (atomic-inc ctr)
				(define old (atomic-box-ref ctr))
				(define new (+ 1 old))
				(define swp (atomic-box-compare-and-swap! ctr old new))
				(if (= old swp) new (atomic-inc ctr)))

			(define start-time (current-time))
			(define (elapsed-secs)
				(define diff (- (current-time) start-time))
				(set! start-time (current-time))
				diff)

			(define cnt-start 0)
			(define (elapsed-count cnt)
				(define diff (- (atomic-box-ref cnt-pairs) cnt-start))
				(set! cnt-start cnt)
				diff)


			(define (right-loop left-item)

				; Check for non-zero counts. A zero here will cause the
				; 'right-wild-logli to throw. The problem here is that
				; every throw gets logged into the logfile (currently, they
				; are not silent) which can sometimes be a huge performance
				; hit. So avoid the throws.
				; Anyway: zero counts means undefined MI.
				(if (< 0 (supobj 'right-count left-item))
					(let ((r-logli (frqobj 'right-wild-logli left-item)))

						; Compute the MI for exactly one pair.
						; Note the sign: it is PLUS log p(x,y)/p(*,y)p(x,*) !!
						; This sign convention agrees with both Yuret and with
						; Wikipedia!
						; logli are defined as -log_2 in object-api.scm,
						; so that's why it looks like the MINUS sign is being
						; used, but it is not.
						(define (do-one-pair right-item)
							(define lipr (LLOBJ 'get-pair left-item right-item))
							(define pr-freq (frqobj 'pair-freq lipr))
							(define pr-logli (frqobj 'pair-logli lipr))

							; It would be nicer to check the count here, not
							; the freq, because the count is an integer. But,
							; unfortunately, we don't know which count this
							; might have come from. (Perhaps it should be
							; cached, when the frequency is computed?)
							(if (< 0 pr-freq)
								(let* ((l-logli (frqobj 'left-wild-logli right-item))
										(fmi (- (+ r-logli l-logli) pr-logli))
										(mi (* pr-freq fmi)))
									(atomic-inc cnt-pairs)
									(frqobj 'set-pair-mi lipr mi fmi)))
							; Return the atom that is holding the MI value.
							lipr)

						; Run the inner loop. The map returns a list of atoms
						; that hold the MI values.
						(CALLBACK (map
							do-one-pair
							(star-obj 'right-duals left-item)))

						; Print some progress statistics.
						(if (eqv? 0 (modulo (atomic-inc cnt-lefties) 10000))
							(let ((secs (elapsed-secs)))
								(format #t
									"Done ~A of ~A outer loops in ~A secs, pairs=~A (~6f pairs/sec)\n"
									(atomic-box-ref cnt-lefties)
									nlefties secs
									(atomic-box-ref cnt-pairs)
									(/ (elapsed-count (atomic-box-ref cnt-pairs)) secs))))
					))
			)

			; This is hog-tied waiting for SQL, running it in parallel
			; provides no speedup.
			; (maybe-par-for-each right-loop lefties)
			(for-each right-loop lefties)

			; Return a count of the number of pairs.
			(atomic-box-ref cnt-pairs)
		)

		; Methods on this class.
		(lambda (message . args)
			(case message
				((cache-pair-mi)        (apply compute-n-cache-pair-mi args))
				(else (apply llobj      (cons message args))))
		))
)

; ---------------------------------------------------------------------

(define-public (make-store LLOBJ)
"
  make-store -- Extend the LLOBJ with additional methods to store
  the left and right wild-card values. The primary utility of this
  class is that it prints a progress report. Its really just a fancy
  wrapper around store-atom, which does the actual work.

  The provided methods are:
  'store-left-marginals - Store all of the left (row) marginal atoms,
       and all of the values attached to them. This also stores the
       wild-wild atom as well.

  'store-right-marginals - Store all of the right (column) marginal
       atoms, and all of the values attached to them. This also stores
       the wild-wild atom as well.

  'store-wildcards - Store both left and right marginals.

  'store-all-elts - Store all non-marginal matrix entries (and the
       attached values, of course).

  'store-all - Store everything pertaining to the matrix: the marginals,
       the matrix entries, and any 'auxilliary' Atoms, if any (that is,
       call the 'store-aux method on the LLOBJ).

  'store-pairs - Store the provided list of Atoms.
"
	(define start-time (current-time))
	(define (elapsed-secs)
		(define diff (- (current-time) start-time))
		(set! start-time (current-time))
		diff)

	(define (store-list XLATE all-atoms CNT MSG)
		(define num-prs (length all-atoms))

		; Create a wrapper around `store-atom` that prints a progress
		; report.  The problem is that millions of pairs may need to be
		; stored, and this just takes a long time.
		(define store-rpt
			(make-progress-rpt store-atom CNT num-prs
				(string-append
					"Stored ~A of ~A " MSG " in ~d secs (~A pairs/sec)\n")))

		(define (xlate atom) (store-rpt (XLATE atom)))

		; Reset the timer.
		(elapsed-secs)

		(maybe-par-for-each
			(lambda (atom) (if (not (null? atom)) (xlate atom)))
			all-atoms)

		(format #t "Done storing ~A ~A in ~A secs\n"
			num-prs MSG (elapsed-secs)))

	; We need 'left-basis, provided by add-pair-stars
	(let ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ)))

		; Store all the wild-card atoms; these are exactly the ones
		; obtained from the object, via the left and right basis.
		(define (store-left-wildcards)
			; Store the wild-wild-card atom, first.
			; This holds the totals for the matrix.
			(store-atom (llobj 'wild-wild))
			(store-list
				(lambda (x) (llobj 'left-wildcard x))
				(star-obj 'right-basis)
				40000 "left-wilds"))

		(define (store-right-wildcards)
			; Store the wild-wild-card atom, first.
			; This holds the totals for the matrix.
			(store-atom (llobj 'wild-wild))
			(store-list
				(lambda (x) (llobj 'right-wildcard x))
				(star-obj 'left-basis)
				40000 "right-wilds"))

		(define (store-all-wildcards)
			(store-left-wildcards)
			(store-right-wildcards))

		; Store the list of given pairs.
		(define (store-pairs all-pairs)
			(store-list (lambda (x) x) all-pairs 100000 "pairs"))

		; Store all elements in the matrix.
		(define (store-all-elts)
			(store-pairs (star-obj 'get-all-elts)))

		; Store everything, including auxilliaries
		(define (store-all)
			(store-all-wildcards)
			(store-all-elts)
			; Not every LLOBJ will have a store-aux,
			; so ignore any error from calling it.
			(catch #t (lambda () (LLOBJ 'store-aux))
				(lambda (key . args) #f))
		)

		; ------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((store-left-marginals) (store-left-wildcards))
				((store-right-marginals)(store-right-wildcards))
				((store-wildcards)      (store-all-wildcards))
				((store-all-elts)       (store-all-elts))
				((store-pairs)          (apply store-pairs args))
				((store-all)            (store-all))
				(else                   (apply llobj (cons message args))))
		))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
(define*-public (batch-all-pair-mi OBJ #:optional (DO-STORE #t))
"
  batch-all-pair-mi LLOBJ [DO-STORE]

  Compute the mutual information between all pairs. Counts, frequencies
  and left, right partial sums are also performed; this is an all-in-one
  routine, which computes all of the needed pre-requisites, and stores
  them, as well as the MI, in the database.

  The mutual information between pairs is described in the overview,
  up top of this file. The access to the pairs is governed by the
  the methods on the passed object.

  Among the things that are computed and stored are the partial sums
  of counts, i.e. the N(x,*) and N(*,y) explained up top, the total
  count N(*,*), the frequencies p(x,y) = N(x,y) / N(*,*), the
  corresponding partial sums.  All of these quantities are written
  back to the database, at the time of computation.

  In order to work correctly, this function assumes that the object
  has at least the minimal low-level API to identify where to find
  the counts on pairs.  This script is designed to work with any kinds
  of pairs.

  Running this script can take hours or longer, depending on the size
  of the dataset. Progress reports are printed to stdout, including
  timing and summary statistics. This script wasn't really designed to
  be efficient; instead, the goal to to allow general, generic knowledge
  representation.  You can compute MI between any kinds of things
  If you just need to count one thing, writing custom scripts that do
  NOT use the atomspace would almost surely be faster.  We put up with
  the performance overhead here in order to get the flexibility that
  the atomspace provides.

  By default, the results of the computation are stored in the currently
  open database. If the optional argument DO-STORE is set to #f, then
  the storage will not be performed.
"
	(define overall-start-time (current-time))
	(define start-time (current-time))
	(define (elapsed-secs)
		(define diff (- (current-time) start-time))
		(set! start-time (current-time))
		diff)

	; Decorate the object with methods that provide wild-cards.
	; All the others get to work off of the basis cached by this one.
	(define wild-obj (add-pair-stars OBJ))

	; Define the object which computes left and right row-lengths
	(define supp-obj (add-support-compute wild-obj))

	; Decorate the object with methods that can compute frequencies.
	(define freq-obj (make-compute-freq wild-obj))

	; Decorate the object with methods that can compute the pair-MI.
	(define batch-mi-obj (make-batch-mi wild-obj))

	; Define the object which will compute row and column subtotals.
	(define subtotal-obj (add-subtotal-mi-compute wild-obj))

	; Define the object which will compute total entropy and MI.
	(define total-obj (add-total-entropy-compute wild-obj))

	; Define the object which will roll up a summary of the supports.
	(define central-obj (make-central-compute wild-obj))

	; Define the object that can store the computed values
	(define store-obj (make-store wild-obj))

	; Optionally store individual atoms
	(define maybe-store-atoms
		(if DO-STORE
			(lambda (atom-list) (for-each store-atom atom-list))
			(lambda (atom-list) #f)))

	(display "Start computing the basis\n")
	(format #t "Support: found num left= ~A num right= ~A in ~A secs\n"
			(length (wild-obj 'left-basis))
			(length (wild-obj 'right-basis))
			(elapsed-secs))

	; First, compute the summations for the left and right wildcard counts.
	; That is, compute N(x,*) and N(*,y) for the supports on x and y.
	(supp-obj 'all-left-marginals)
	(supp-obj 'all-right-marginals)

	(format #t "Done with wild-card count N(x,*) and N(*,y) in ~A secs\n"
		(elapsed-secs))

	(format #t "Total count N(*,*) = ~A = ~A\n"
		((add-support-api OBJ) 'total-count-left)
		((add-support-api OBJ) 'total-count-right))

	; May as well get the support averages out of the way, too.
	(central-obj 'cache-left)
	(central-obj 'cache-right)

	; Compute the pair-frequencies, and the left and right
	; wildcard frequencies and log-frequencies.
	(freq-obj 'init-freq)

	(display "Going to do individual pair frequencies.\n")
	(let ((pair-cnt (freq-obj 'cache-all-pair-freqs)))
		(format #t "Done computing ~A pair frequencies in ~A secs\n"
				pair-cnt (elapsed-secs)))

	(display "Start computing log P(*,y).\n")
	(freq-obj 'cache-all-left-freqs)
	(format #t "Done computing ~A left-wild log frequencies in ~A secs.\n"
		(length (wild-obj 'right-basis)) (elapsed-secs))

	(display "Done with -log P(*,y), start -log P(x,*)\n")
	(freq-obj 'cache-all-right-freqs)
	(format #t "Done computing ~A right-wild log frequencies in ~A secs.\n"
		(length (wild-obj 'left-basis)) (elapsed-secs))

	(if DO-STORE (begin
		(store-obj 'store-wildcards)
		(display "Done saving -log P(x,*) and P(*,y)\n")))

	; Now, the individual pair mi's
	(if DO-STORE
		(display "Going to compute and store individual pair MI.\n")
		(display "Going to compute individual pair MI.\n"))
	(elapsed-secs)
	(let ((num-prs (batch-mi-obj 'cache-pair-mi maybe-store-atoms)))

		; This print triggers as soon as the let above finishes.
		(format #t "Done computing ~A pair MI's in ~A secs\n"
			num-prs (elapsed-secs))
	)

	(display "Going to do column and row subtotals.\n")
	(subtotal-obj 'cache-all-left-entropy)
	(subtotal-obj 'cache-all-right-entropy)
	(subtotal-obj 'cache-all-left-mi)
	(subtotal-obj 'cache-all-right-mi)

	(display "Going to compute the left, right and total entropy.\n")
	(total-obj 'cache-entropy)
	(total-obj 'cache-mi)

	(if DO-STORE (begin
		(display "Done computing totals; start saving wildcards.\n")
		(store-obj 'store-wildcards))
		(display "Done computing totals.\n"))

	(format #t "Finished with MI computations; this took ~5f hours.\n"
		(/ (- (current-time) overall-start-time) 3600.0))
)

; ---------------------------------------------------------------------
