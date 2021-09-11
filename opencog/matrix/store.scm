;
; store.scm
;
; Store a matrix, or marginals, or both/everything.
;
; Copyright (c) 2013, 2014, 2017, 2018 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The object below provides a way of storing selective portions
; of a matrix.
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

(define*-public (make-store LLOBJ #:optional (STORAGE #f))
"
  make-store LLOBJ [STORAGE] -- Extend the LLOBJ with additional
  methods to store the left and right wild-card values. If the STORAGE
  argument is provided, then Atoms will be stored there, else the
  default storage will be used. If the STORAGE is provided, it must
  be a StorageNode, and it must be open. If its not provided, then
  the AtomSpace must be connected to default storage that is open.

  The primary utility of this class is that it prints a progress report.
  (That is, its not hard to write a simple loop over all atoms in a
  matrix.) So this is really just a fancy wrapper around store-atom,
  which does the actual work.

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
	(define (do-store-atom ATM)
		(if STORAGE (store-atom ATM STORAGE) (store-atom ATM)))

	(define (store-list XLATE all-atoms CNT MSG)
		(define num-prs (length all-atoms))

		; Create a wrapper around `store-atom` that prints a progress
		; report.  The problem is that millions of pairs may need to be
		; stored, and this just takes a long time.
		(define store-rpt
			(make-progress-rpt do-store-atom CNT num-prs
				(string-append
					"Stored ~A of ~A " MSG " in ~d secs (~A pairs/sec)\n")))

		(define (xlate atom) (store-rpt (XLATE atom)))

		(define elapsed-secs (make-elapsed-secs))

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
			(do-store-atom (llobj 'wild-wild))
			(store-list
				(lambda (x) (llobj 'left-wildcard x))
				(star-obj 'right-basis)
				200000 "left-wilds"))

		(define (store-right-wildcards)
			; Store the wild-wild-card atom, first.
			; This holds the totals for the matrix.
			(do-store-atom (llobj 'wild-wild))
			(store-list
				(lambda (x) (llobj 'right-wildcard x))
				(star-obj 'left-basis)
				200000 "right-wilds"))

		(define (store-all-wildcards)
			(store-left-wildcards)
			(store-right-wildcards))

		; Store the list of given pairs.
		(define (store-pairs all-pairs)
			(store-list (lambda (x) x) all-pairs 200000 "pairs"))

		; Store all elements in the matrix.
		(define (store-all-elts)
			; The 'get-all-elts method can take a very long time
			; on big objects, especially when they are filtered.
			; So report that time.
			(define elap (make-elapsed-secs))
			(define all-prs (star-obj 'get-all-elts))
			(format #t "Found ~A pairs in ~A secs\n"
				(length all-prs) (elap))
			(store-pairs all-prs))

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
