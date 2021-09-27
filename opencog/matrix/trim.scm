;
; trim.scm
;
; Like filter.scm, but removes Atoms outright from the AtomSpace.
; The goal is to be a bit more CPU and storage efficient.
;
; XXX TODO
; -- finish writing documentation
; -- move to the (opencog matrix) module
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------

(define (make-elapsed-secs)
   (define start-time (current-time))
   (lambda ()
      (define now (current-time))
      (define diff (- now start-time))
      (set! start-time now)
      diff)
)

; ---------------------------------------------------------------------

(define-public (trim-matrix LLOBJ
	LEFT-BASIS-PRED RIGHT-BASIS-PRED PAIR-PRED)
"
  trim-matrix LLOBJ LEFT-BASIS-PRED RIGHT-BASIS-PRED ELEMENT-PRED
  Remove (delete) Atoms from the AtomSpace that pass the predicates.
  If storage is connected, then these are removed from storage too.

  LLOBJ should be an object with the conventional matrix methods on it.

  LEFT-BASIS-PRED should be a function taking a ROW as an argument; it
      should return #t if that row is to be deleted, else it returns #f.

  RIGHT-BASIS-PRED should be a function taking a COL as an argument; it
      should return #t if that column is to be deleted, else it returns #f.

  ELEMENT-PRED should be a function taking a matrix element as an
      argument; it should return #t if that matrix element should be
      deleted, else should returns #f.
"
	(define star-obj (add-pair-stars LLOBJ))
	(define elapsed-secs (make-elapsed-secs))

	; After removing pairs, it may now happen that there are left
	; and right basis elements that are no longer in any pairs.
	; Remove these too.
	(define (trim-type BASIS-LIST)
		(define party (star-obj 'pair-type))
		(for-each
			(lambda (base)
				(if (and (cog-atom? base)
						(equal? 0 (cog-incoming-size-by-type base party)))
					(cog-delete! base)))
			BASIS-LIST))

	; Walk over the left and right basis.
	; The use of cog-delete-recursive! may knock out other
	; elements in the matrix, and so `cog-atom?` is used
	; to see if that particular entry still exists.
	(for-each
		(lambda (base)
			(if (and (cog-atom? base) (not (LEFT-BASIS-PRED base)))
				(cog-delete-recursive! base)))
		(star-obj 'left-basis))

	(format #t "Trimmed left basis in ~A seconds.\n" (elapsed-secs))

	(for-each
		(lambda (base)
			(if (and (cog-atom? base) (not (RIGHT-BASIS-PRED base)))
				(cog-delete-recursive! base)))
		(star-obj 'right-basis))

	(format #t "Trimmed right basis in ~A seconds.\n" (elapsed-secs))

	; Walk over the list of all entries and just delete them.
	(for-each
		(lambda (atom)
			(if (and (cog-atom? atom) (not (PAIR-PRED atom)))
				(cog-delete-recursive! atom)))
		(star-obj 'get-all-elts))

	; After removing pairs, it may now happen that there are left
	; and right basis elements that are no longer in any pairs.
	; Remove these too.
	(trim-type (star-obj 'left-basis))
	(trim-type (star-obj 'right-basis))

	; Trimming has commited violence to the matrix. Let it know.
	(if (LLOBJ 'provides 'clobber) (LLOBJ 'clobber))

	(format #t "Trimmed all pairs in ~A seconds.\n" (elapsed-secs))
)


(define-public (subtotal-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT)
"
  subtotal-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT
  Remove (delete) Atoms from the AtomSpace whose counts are at or below
  the indicated cutoffs. If storage is connected, then these are removed
  from storage too.

  This function will fail to work correctly, if support marginals have
  not been computed on LLOBJ. That is because the support marginals are
  used to obtain subtotal counts for rows and columns.  To compute
  support marginals, say `((add-support-compute LLOBJ) 'cache-all)`

  LLOBJ should be an object with the conventional matrix methods on it.

  LEFT-CUT should be a number; if the count on that row is equal or less
      than this number, then the entire row (including marginals) will
      be deleted.
"
	(define stars-obj (add-pair-stars LLOBJ))
	(define sup-obj (add-support-api stars-obj))

	; ---------------
	; Remove rows and columns that are below-count.
	;
	; Yes, we want LEFT-CUT < right-wild-count this looks weird,
	; but is correct: as LEFT-CUT gets larger, the size of the
	; left-basis shrinks.
	(define (left-basis-pred ITEM)
		(< LEFT-CUT (sup-obj 'right-count ITEM)))

	(define (right-basis-pred ITEM)
		(< RIGHT-CUT (sup-obj 'left-count ITEM)))

	(define (pair-pred PAIR)
		(< PAIR-CUT (LLOBJ 'get-count PAIR)))

	; ---------------
	(trim-matrix stars-obj
		left-basis-pred right-basis-pred pair-pred)
)


(define-public (support-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT)
"
  support-trim LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT

  Almost exactly identical to subtotal-trim, except that the cuts apply
  to the size of the support, rather than to the count.
"
	(define stars-obj (add-pair-stars LLOBJ))
	(define sup-obj (add-support-api stars-obj))

	; ---------------
	; Remove rows and columns that are below-count.
	;
	; Yes, we want LEFT-CUT < right-support this looks weird,
	; but is correct: as LEFT-CUT gets larger, the size of the
	; left-basis shrinks.
	(define (left-basis-pred ITEM)
		(< LEFT-CUT (sup-obj 'right-support ITEM)))

	(define (right-basis-pred ITEM)
		(< RIGHT-CUT (sup-obj 'left-support ITEM)))

	(define (pair-pred PAIR)
		(< PAIR-CUT (LLOBJ 'get-count PAIR)))

	; ---------------
	(trim-matrix stars-obj
		left-basis-pred right-basis-pred pair-pred)
)
