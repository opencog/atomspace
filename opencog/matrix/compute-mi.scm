;
; compute-mi.scm
;
; Compute the mutual information of pairs of items.
;
; Copyright (c) 2013, 2014, 2017 Linas Vepstas
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
;   'item-pair, which should return the EvaluationLink, given the
;        ListLink
;   'left-wildcard and 'right-wildcard, indicating where the partial
;        sums, such as N(x,*) and N(*,y) should be stored.

; Let N(wl,wr) denote the number of times that the pair (wl, wr) has
; actually been observed; that is, N("some-word", "other-word") for the
; example above.  Properly speaking, this count is conditioned on the
; LinkGrammarRelationshipNode "ANY", so the correct notation would be
; N(rel, wl, wr) with `rel` the relationship.  In what follows, the
; relationship is always assumed to be the same, and is thus dropped.
; (the relationship is provided through the GET-PAIR functiion).
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
; These sums are computed, for a given item, by the `make-compute-count`
; object defined below.  It attached these counts at the locations
; provided by the underlying object. By default, thse are given by
; `add-pair-count-api` object, althought these are designed to be
; overloaded, if needed.

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
; fetched with the 'left-wild-count and 'right-wild-count methods on
; the object.  The value for N(*,*) can be gotten with the
; 'wild-wild-count method.
;
; The fractional mutual information for the pair (x,y) is defined as
;
;     MI(x,y) = - log_2 [ p(x,y) /  p(x,*) p(*,y) ]
;
; This is computed by the script batch-all-pair-mi below. The value is
; attached at the location provided by the 'set-pair-mi method on the
; object.  It can later be retrieved with the corresponding 'pair-mi
; method.
;
; The main utility wrapper not only computes all of these, but also
; stores them in the database, from which they can be retreived at some
; future date.  This helps avoid repeating the calculation, which can
; take many hours for large datasets (tens of millions of pairs).
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
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
; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to compute wildcard counts
; for pairs, and store the results using the count-object API.
; That is, compute the summations N(x,*) = sum_y N(x,y) where (x,y)
; is a pair, and N(x,y) is the count of how often that pair has been
; observed, and * denotes the wild-card, ranging over all items
; supported in that slot.
;
(define (make-compute-count LLOBJ)

	; We need 'left-basis, provided by add-pair-stars
	; We need 'set-left-wild-count, provided by add-pair-count-api
	(let ((llobj LLOBJ)
			(cntobj (add-pair-count-api LLOBJ))
			(star-obj (add-pair-stars LLOBJ)))

		; Compute the left-side wild-card count. This is the number
		; N(*,y) = sum_x N(x,y) where ITEM==y and N(x,y) is the number
		; of times that the pair (x,y) was observed.
		; This returns the count, or zero, if the pair was never observed.
		(define (compute-left-count ITEM)
			(fold
				(lambda (pr sum) (+ sum (llobj 'pair-count pr)))
				0
				(star-obj 'left-stars ITEM)))

		; Compute and cache the left-side wild-card counts N(*,y).
		; This returns the atom holding the cached count, thus
		; making it convient to persist (store) this cache in
		; the database. It returns nil if the count was zero.
		(define (cache-left-count ITEM)
			(define cnt (compute-left-count ITEM))
			(if (< 0 cnt)
				(cntobj 'set-left-wild-count ITEM cnt)
				'()))

		; Compute the right-side wild-card count N(x,*).
		(define (compute-right-count ITEM)
			(fold
				(lambda (pr sum) (+ sum (llobj 'pair-count pr)))
				0
				(star-obj 'right-stars ITEM)))

		; Compute and cache the right-side wild-card counts N(x,*).
		; This returns the atom holding the cached count, or nil
		; if the count was zero.
		(define (cache-right-count ITEM)
			(define cnt (compute-right-count ITEM))
			(if (< 0 cnt)
				(cntobj 'set-right-wild-count ITEM cnt)
				'()))

		; Compute and cache all of the left-side wild-card counts.
		; This computes N(*,y) for all y, in parallel.
		;
		; This method returns a list of all of the atoms holding
		; those counts; handy for storing in a database.
		(define (cache-all-left-counts)
			(map cache-left-count (star-obj 'right-basis)))

		(define (cache-all-right-counts)
			(map cache-right-count (star-obj 'left-basis)))

		; Compute the total number of times that all pairs have been
		; observed. In formulas, return
		;     N(*,*) = sum_x N(x,*) = sum_x sum_y N(x,y)
		;
		; This method assumes that the partial wild-card counts have
		; been previously computed and cached.  That is, it assumes that
		; the 'right-wild-count returns a valid value, which really
		; should be the same value as 'compute-right-count on this object.
		(define (compute-total-count-from-left)
			(fold
				;;; (lambda (item sum) (+ sum (compute-right-count item)))
				(lambda (item sum) (+ sum (cntobj 'right-wild-count item)))
				0
				(star-obj 'left-basis)))

		; Compute the total number of times that all pairs have been
		; observed. That is, return N(*,*) = sum_y N(*,y). Note that
		; this should give exactly the same result as the above; however,
		; the order in which the sums are performed is distinct, and
		; thus any differences indicate a bug.
		(define (compute-total-count-from-right)
			(fold
				;;; (lambda (item sum) (+ sum (compute-left-count item)))
				(lambda (item sum) (+ sum (cntobj 'left-wild-count item)))
				0
				(star-obj 'right-basis)))

		; Compute the total number of times that all pairs have been
		; observed. That is, return N(*,*).  Throws an error if the
		; left and right summations fail to agree.
		(define (compute-total-count)
			(define l-cnt (compute-total-count-from-left))
			(define r-cnt (compute-total-count-from-right))

			; The left and right counts should be equal!
			; XXX fixme, allow for small rounding errors.
			(if (not (eqv? l-cnt r-cnt))
				(throw 'bad-summation 'count-all-pairs
					(format #f "Error: pair-counts unequal: ~A ~A\n" l-cnt r-cnt)))
			l-cnt)

		; Compute and cache the total observation count for all pairs.
		; This returns the atom holding the cached count.
		(define (cache-total-count)
			(define cnt (compute-total-count))
			(cntobj 'set-wild-wild-count cnt))

		; Methods on this class.
		(lambda (message . args)
			(case message
				((compute-left-count)     (apply compute-left-count args))
				((cache-left-count)       (apply cache-left-count args))
				((compute-right-count)    (apply compute-right-count args))
				((cache-right-count)      (apply cache-right-count args))
				((cache-all-left-counts)  (cache-all-left-counts))
				((cache-all-right-counts) (cache-all-right-counts))
				((compute-total-count)    (compute-total-count))
				((cache-total-count)      (cache-total-count))
				(else (apply llobj        (cons message args))))
		))
)

; ---------------------------------------------------------------------
;
; Extend the LLOBJ with additional methods to compute observation
; frequencies and entropies for pairs, including partial-sum entropies
; (mutual information) for the left and right side of each pair.
; This will also cache the results of these computations in a
; standardized location.
;
; The LLOBJ must have valid left and right wild-card counts on it.
; These need to have been previously computed, before methods on
; this class are called.
;
; Before using this class, the 'init-freq method must be called,
; and it must be called *after* a valid wild-wild count is available.

(define (make-compute-freq LLOBJ)

	; We need 'left-basis, provided by add-pair-stars
	; We need 'wild-wild-count, provided by add-pair-count-api
	; We need 'set-left-wild-freq, provided by add-pair-freq-api
	; We need 'set-size, provided by add-report-api
	(let ((llobj LLOBJ)
			(cntobj (add-pair-count-api LLOBJ))
			(frqobj (add-pair-freq-api LLOBJ))
			(wldobj (add-pair-stars LLOBJ))
			(rptobj (add-report-api LLOBJ))
			(tot-cnt 0))

		(define (init)
			(set! tot-cnt (cntobj `wild-wild-count)))

		; Compute the pair frequency P(x,y) = N(x,y) / N(*,*)  This is
		; the frequency with which the pair (x,y) is observed. Return
		; the frequency, or zero, if the pair was never observed.
		(define (compute-pair-freq PAIR)
			(/ (cntobj 'pair-count PAIR) tot-cnt))

		; Compute the left-side wild-card frequency. This is the ratio
		; P(*,y) = N(*,y) / N(*,*) = sum_x P(x,y)
		(define (compute-left-freq ITEM)
			(/ (cntobj 'left-wild-count ITEM) tot-cnt))
		(define (compute-right-freq ITEM)
			(/ (cntobj 'right-wild-count ITEM) tot-cnt))

		; Compute and cache the pair frequency.
		; This returns the atom holding the cached count, thus
		; making it convient to persist (store) this cache in
		; the database. It returns nil if the count was zero.
		(define (cache-pair-freq PAIR)
			(define freq (compute-pair-freq PAIR))
			(if (< 0 freq)
				(frqobj 'set-pair-freq PAIR freq)
				'()))

		; Compute and cache the left-side wild-card frequency.
		(define (cache-left-freq ITEM)
			(define freq (compute-left-freq ITEM))
			(if (< 0 freq) (frqobj 'set-left-wild-freq ITEM freq)))

		(define (cache-right-freq ITEM)
			(define freq (compute-right-freq ITEM))
			(if (< 0 freq) (frqobj 'set-right-wild-freq ITEM freq)))

		; Compute and cache all of the pair frequencies.
		; This computes P(x,y) for all (x,y)
		; This returns a count of the pairs.
		; Also caches the total dimensions of the matrix.
		(define (cache-all-pair-freqs)
			(define cnt 0)
			(define lefties (wldobj 'left-basis))
			(define left-size (length lefties))
			(define right-size (length (wldobj 'right-basis)))

			; The outer-loop.
			(define (right-loop left-item)
				(for-each
					(lambda (pr)
						(cache-pair-freq pr)
						(set! cnt (+ cnt 1)))
					(wldobj 'right-stars left-item)))

			(for-each right-loop lefties)

			; Save the total size of the thing.
			(rptobj 'set-size left-size right-size cnt)

			; Return the total.
			cnt)

		; Compute and cache all of the left-side frequencies.
		; This computes P(*,y) for all y, in parallel.
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
	; We need 'set-pair-mi, provided by add-pair-freq-api
	; We need 'right-wild-count, provided by add-pair-count-api
	(let ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ))
			(cntobj (add-pair-count-api LLOBJ))
			(frqobj (add-pair-freq-api LLOBJ)))

		; Loop over all pairs, computing the MI for each. The loop
		; is actually two nested loops, with a loop over the
		; left-basis on the outside, and over right-stars for
		; the inner loop. This returns a list of all atoms holding
		; the MI, suitable for iterating for storage.
		(define (compute-n-cache-pair-mi)
			(define all-atoms '())
			(define lefties (star-obj 'left-basis))

			; progress stats
			(define cnt-pairs 0)
			(define cnt-lefties 0)
			(define nlefties (length lefties))

			(define start-time (current-time))
			(define (elapsed-secs)
				(define diff (- (current-time) start-time))
				(set! start-time (current-time))
				diff)

			(define cnt-start 0)
			(define (elapsed-count cnt)
				(define diff (- cnt-pairs cnt-start))
				(set! cnt-start cnt)
				diff)


			(define (right-loop left-item)

				; Check for non-zero counts. A zero here will cause the
				; 'right-wild-logli to throw. The problem here is that
				; every throw gets logged into the logfile (currently, they
				; are not silent) which can sometimes be a huge performance
				; hit. So avoid the throws.
				; Anyway: zero counts means undefined MI.
				(if (< 0 (cntobj 'right-wild-count left-item))
					(let ((r-logli (frqobj 'right-wild-logli left-item)))

						; Compute the MI for exactly one pair.
						(define (do-one-pair lipr)
							(define pr-freq (frqobj 'pair-freq lipr))
							(define pr-logli (frqobj 'pair-logli lipr))

							(define right-item (gdr lipr))
							(if (< 0 (cntobj 'left-wild-count right-item))
								(let* ((l-logli (frqobj 'left-wild-logli right-item))
										(fmi (- pr-logli (+ r-logli l-logli)))
										(mi (* pr-freq fmi))
										(atom (frqobj 'set-pair-mi lipr mi fmi)))
									(set! all-atoms (cons atom all-atoms))
									(set! cnt-pairs (+ cnt-pairs 1)))))

						; Run the inner loop
						(for-each
							do-one-pair
							(star-obj 'right-stars left-item))

						; Print some progress statistics.
						(set! cnt-lefties (+ cnt-lefties 1))
						(if (eqv? 0 (modulo cnt-lefties 10000))
							(let ((secs (elapsed-secs)))
								(format #t
									"Done ~A of ~A outer loops in ~A secs, pairs=~A (~6f pairs/sec)\n"
									cnt-lefties nlefties secs cnt-pairs
									(/ (elapsed-count cnt-pairs) secs))))
					))
			)

			;; XXX Maybe FIXME This could be a par-for-each, to run the
			; calculations in parallel, but then we need to make the
			; all-atoms list thread-safe.  Two problems: one is that
			; current guile par-for-each implementation sucks.
			; The other is that the atom value-fetching is done under
			; a global lock, thus effectively single-threaded.
			(for-each right-loop lefties)

			; Return the list of ALL atoms with MI on them
			all-atoms
		)

		; Methods on this class.
		(lambda (message . args)
			(case message
				((cache-pair-mi)        (compute-n-cache-pair-mi))
				(else (apply llobj      (cons message args))))
		))
)

; ---------------------------------------------------------------------

(define-public (make-store LLOBJ)
"
  make-store -- Extend the LLOBJ with addtional methods to store
  the left and right wild-card values.
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

		(for-each
			(lambda (atom) (if (not (null? atom)) (xlate atom)))
			all-atoms)

		(format #t "Done storing ~A ~A in ~A secs\n"
			num-prs MSG (elapsed-secs)))

	; We need 'left-basis, provided by add-pair-stars
	(let ((llobj LLOBJ)
			(star-obj (add-pair-stars LLOBJ)))

		; Store all the wild-card atoms; these are exactly the ones
		; obtained from the object, via the left and right basis.
		(define (store-all-wildcards)
			; Store the wild-wild-card atom, first.
			; This holds the totals for the matrix.
			(store-atom (llobj 'wild-wild))

			(store-list
				(lambda (x) (llobj 'left-wildcard x))
				(star-obj 'right-basis)
				40000 "left-wilds")

			(store-list
				(lambda (x) (llobj 'right-wildcard x))
				(star-obj 'left-basis)
				40000 "right-wilds")
		)

		; Store all the pairs. These must be provided as a list to us,
		; because, at this time, we don't hve an effective way of working
		; with the non-zero elements.  Maybe a better solution will become
		; clear over time...
		(define (store-pairs all-pairs)
			(store-list (lambda (x) x) all-pairs 100000 "pairs"))

		; ------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((store-wildcards)      (store-all-wildcards))
				((store-pairs)          (apply store-pairs args))
				(else                   (apply llobj (cons message args))))
		))
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; Compute the mutual information between all pairs. Counts, frequencies
; and left, right partial sums are also performed; this is an all-in-one
; routine, which computes all of the needed pre-requisites, and stores
; them, as well as the MI, in the database.
;
; The mutual information between pairs is described in the overview,
; up top of this file. The access to the pairs is governed by the
; the methods on the passed object.
;
; Among the things that are computed and stored are the partial sums
; of counts, i.e. the N(x,*) and N(*,y) explained up top, the total
; count N(*,*), the frequencies p(x,y) = N(x,y) / N(*,*), the
; corresponding partial sums.  All of these quantities are written
; back to the database, at the time of computation.
;
; In order to work correctly, this function assumes that the object
; has at least the minimal low-level API to identify where to find
; the counts on pairs.  This script is designed to work with any kinds
; of pairs.
;
; Running this script can take hours or longer, depending on the size
; of the dataset. Progress reports are printed to stdout, including
; timing and summary statistics. This script wasn't really designed to
; be efficient; instead, the goal to to allow general, generic knowledge
; representation.  You can compute MI between any kinds of things
; If you just need to count one thing, writing custom scripts that do
; NOT use the atomspace would almost surely be faster.  We put up with
; the performance overhead here in order to get the flexibility that
; the atomspace provides.
;
(define-public (batch-all-pair-mi OBJ)

	(define overall-start-time (current-time))
	(define start-time (current-time))
	(define (elapsed-secs)
		(define diff (- (current-time) start-time))
		(set! start-time (current-time))
		diff)

	; Decorate the object with methods that report support.
	; All the others get to work off of the basis cached by this one.
	(define wild-obj (add-pair-stars OBJ))

	; Decorate the object with methods that can compute counts.
	(define count-obj (make-compute-count wild-obj))

	; Decorate the object with methods that can compute frequencies.
	(define freq-obj (make-compute-freq wild-obj))

	; Decorate the object with methods that can compute the pair-MI.
	(define batch-mi-obj (make-batch-mi wild-obj))

	; Define the object which will compute row and column subtotals.
	(define subtotal-obj (add-subtotal-mi-compute wild-obj))

	; Define the object which will compute total entropy and MI.
	(define total-obj (add-total-entropy-compute wild-obj))

	; Define the object which computes left and right row-lengths
	(define supp-obj (add-support-compute wild-obj))

	; Define the object which will roll up a summary of the supports.
	(define central-obj (make-central-compute wild-obj))

	; Define the object that can store the computed values
	(define store-obj (make-store wild-obj))

	(display "Start computing the basis\n")
	(format #t "Support: found num left= ~A num right= ~A in ~A secs\n"
			(length (wild-obj 'left-basis))
			(length (wild-obj 'right-basis))
			(elapsed-secs))

	; First, compute the summations for the left and right wildcard counts.
	; That is, compute N(x,*) and N(*,y) for the supports on x and y.

	(count-obj 'cache-all-left-counts)
	(count-obj 'cache-all-right-counts)

	(format #t "Done with wild-card count N(x,*) and N(*,y) in ~A secs\n"
		(elapsed-secs))

	; Now, compute the grand-total.
	(count-obj 'cache-total-count)
	(format #t "Done computing N(*,*) total-count= ~A in ~A secs\n"
		((add-pair-count-api OBJ) 'wild-wild-count)
		(elapsed-secs))

	; Compute the pair-frequencies, and the left and right
	; wildcard frequencies and log-frequencies.
	(freq-obj 'init-freq)

	(display "Going to do individual pair frequencies\n")
	(let ((pair-cnt (freq-obj 'cache-all-pair-freqs)))
		(format #t "Done computing ~A pairs in ~A secs\n"
				pair-cnt (elapsed-secs)))

	(display "Start computing log P(*,y)\n")
	(freq-obj 'cache-all-left-freqs)
	(format #t "Done computing ~A left-wilds in ~A secs\n"
		(length (wild-obj 'right-basis)) (elapsed-secs))

	(display "Done with -log P(*,y), start -log P(x,*)\n")
	(freq-obj 'cache-all-right-freqs)
	(format #t "Done computing ~A right-wilds in ~A secs\n"
		(length (wild-obj 'left-basis)) (elapsed-secs))

	(store-obj 'store-wildcards)
	(display "Done computing and saving -log P(x,*) and P(*,y)\n")

	; Now, the individual pair mi's
	(display "Going to do individual pair MI\n")
	(elapsed-secs)
	(let* ((all-atoms (batch-mi-obj 'cache-pair-mi))
			(num-prs (length all-atoms)))

		; This print triggers as soon as the let* above finishes.
		(format #t "Done computing ~A pair MI's in ~A secs\n"
			num-prs (elapsed-secs))

		(store-obj 'store-pairs all-atoms)
	)

	(display "Going to do column and row subtotals\n")
	(subtotal-obj 'cache-all-subtotals)
	(supp-obj 'cache-all)

	(display "Going to compute the left, right and total entropy\n")
	(total-obj 'cache-entropy)
	(total-obj 'cache-mi)
	(central-obj 'cache-all)

	(display "Done computing totals; start saving wildcards\n")
	(store-obj 'store-wildcards)

	(format #t "Finished with MI computations; this took ~4f hours\n"
		(/ (- (current-time) overall-start-time) 3600.0))
)

; ---------------------------------------------------------------------
