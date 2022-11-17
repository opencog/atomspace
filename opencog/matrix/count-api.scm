;
; count-api.scm
;
; Define object-oriented class API that handles counting.
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
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog))

; ---------------------------------------------------------------------

;
;     ; Return the observed count for the pair PAIR.
;     (define (get-count PAIR)
;        (cog-value-ref (cog-value PAIR (Predicate "counter")) 42))
;
;     ; Return the observed count for the pair (L-ATOM, R-ATOM), if it
;     ; exists, else return zero.
;     (define (get-pair-count L-ATOM R-ATOM)
;        (define stats-atom (get-pair L-ATOM R-ATOM))
;        (if (nil? stats-atom) 0 (get-count stats-atom)))
;
;              ((pair-count) get-pair-count)
;              ((get-count) get-count)


(define-public (add-pair-count LLOBJ)
"
  add-pair-count LLOBJ - Extend LLOBJ with methods to get, set and
  increment the counts on pairs.

  The supported methods are:

  'pair-count L R - Returns the total observed count on the pair (L,R)
      L must be an Atom of type 'left-type on the base object LLOBJ,
      and likewise for R. Returns zero if such a pair does not exist.

  'get-count P - Returns the total observed count on the pair P.
      The P atom should be one of the atoms returned by the LLOBJ
      'get-pair method.
  'set-count P - Set the total observed count on the pair P.

  'inc-count P N - Perform an atomic increment of the count on P by N.
       The increment is atomic, meaning that it is thread-safe against
       racing threads.

  'move-count ACC DONOR FRAC - Move a fraction FRAC of the count from
       DONOR to ACC. The move is atomic, in that no counts are lost in
       the case of racing threads performing other count updates.
	    ACC and DONOR should be two pairs in this matrix.
	    FRAC should be a numeric fraction, between 0.0 and 1.0.
"
	; Accumulate a fraction FRAC of the count from DONOR into ACC.
	(define (move-count ACCUM DONOR FRAC)
		; Return #t if the count is effectively zero.
		; Use an epsilon for rounding errors.
		(define (is-zero? cnt) (< cnt 1.0e-10))

		; The counts on the accumulator and the pair to merge.
		(define donor-cnt (LLOBJ 'get-count DONOR))
		(define frac-cnt (* FRAC donor-cnt))

		; If there is nothing to transfer over, do nothing.
		(when (not (is-zero? frac-cnt))
			(inc-count ACCUM frac-cnt)
			(inc-count DONOR (- frac-cnt)))

		; Return how much was transferred over.
		frac-cnt)

	; -------------------------------------------------------
	; Return default, only if LLOBJ does not provide symbol
	(define (overload symbol default)
		(define fp (LLOBJ 'provides symbol))
		(if fp fp default))

	;-------------------------------------------

	(define (help)
		(format #t
			(string-append
"This is the `add-pair-count` object applied to the \"~A\"\n"
"object.  It provides generic counting support methods for the base\n"
"object. This is a core utility, widely used to simplify counting. For\n"
"more information, say `,d add-pair-count` or `,describe add-pair-count`\n"
"at the guile prompt, or just use the 'describe method on this object.\n"
"You can also get at the base object with the 'base method: e.g.\n"
"`((obj 'base) 'help)`.\n"
)
			(LLOBJ 'id)))

	(define (describe)
		(display (procedure-property add-pair-count 'documentation)))

	;-------------------------------------------

	; Provide default methods, but only if the low-level object
	; does not already provide them.
	(define f-move-count       (overload 'move-count move-count))

	;-------------------------------------------
	; Explain what is provided.
	(define (provides meth)
		(case meth
			((move-count)       f-move-count)

			(else               (LLOBJ 'provides meth))))

	;-------------------------------------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((move-count)       (apply f-move-count args))

			((provides)         (apply provides args))
			((help)             (help))
			((describe)         (describe))
			((obj)              "add-pair-count")
			((base)             LLOBJ)
			(else               (apply LLOBJ (cons message args))))
	))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
