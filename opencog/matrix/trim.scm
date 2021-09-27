;
; trim.scm
;
; Like filter.scm, but removes Atoms outright from the AtomSpace.
; The goal is to be a bit more CPU and storage efficient.
;
; XXX TODO
; -- finish writing documentation
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))

; ---------------------------------------------------------------------

(define-public (add-trimmer LLOBJ)
"
  add-trimmer LLOBJ - Assorted methods for removing matrix entries,
  rows and columns. Removed entries are removed (deleted) from the
  AtomSpace.  If storage is connected, then these are removed from
  storage, too.

  LLOBJ should be an object with the conventional matrix methods on it.

  The methods on this object provide functions that resemble those of
  the `add-generic-filter`, `add-subtotal-filter`, and
  `add-zero-filter`, except that this object simply deletes those atoms,
  instead of filtering them out of the object API. By using the trimmer,
  the total size of the dataset will in general be smaller, and the
  access to the matrix entries will be faster.  This is the primary
  advantage of using the trimmer over use the filters.

  The provided methods are:

  'generic-trim LEFT-BASIS-PRED RIGHT-BASIS-PRED ELEMENT-PRED
      Remove (delete) Atoms from the AtomSpace that pass the predicates.
      If storage is connected, then these are removed from storage too.

      LEFT-BASIS-PRED should be a function taking a ROW as an argument;
      it should return #t if that row is to be deleted, else it returns #f.

      RIGHT-BASIS-PRED should be a function taking a COL as an argument;
      it should return #t if that column is to be deleted, else it
      returns #f.

      ELEMENT-PRED should be a function taking a matrix element as an
      argument; it should return #t if that matrix element should be
      deleted, else it should return #f.

  'subtotal-trim LEFT-CUT RIGHT-CUT PAIR-CUT
      Remove (delete) Atoms from the AtomSpace whose counts are at or
      below the indicated cutoffs. If storage is connected, then these
      are removed from storage too.

      This method will fail to work correctly, if support marginals
      have not been computed on LLOBJ. That is because the support
      marginals are used to obtain subtotal counts for rows and
      columns. To compute support marginals, say
      `((add-support-compute LLOBJ) 'cache-all)`

      LEFT-CUT should be a number; if the count on that row is equal or
          less than this number, then the entire row (including marginals)
          will be deleted.

      RIGHT-CUT should be a number; if the count on that column is equal
          or less than this number, then the entire row (including
          marginals) will be deleted.

      PAIR-CUT should be a number; if the count on that matrix element
          is equal or less than this number, then that matrix element
          will be deleted.

  'support-trim LEFT-CUT RIGHT-CUT PAIR-CUT
      Almost exactly identical to 'subtotal-trim, except that the cuts
      apply to the size of the support of a column or row, rather than
      to the count.  This is particularly useful when a matrix might
      have only one matrix entry in a given row or column, no matter how
      big the count on that entry may be.
"
	(define star-obj (add-pair-stars LLOBJ))

	(define (trim-generic OBJ LEFT-BASIS-PRED RIGHT-BASIS-PRED PAIR-PRED)

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
		(if (OBJ 'provides 'clobber) (OBJ 'clobber))

		(format #t "Trimmed all pairs in ~A seconds.\n" (elapsed-secs))
	)

	(define (generic-trim LEFT-BASIS-PRED RIGHT-BASIS-PRED PAIR-PRED)
		(trim-generic star-obj LEFT-BASIS-PRED RIGHT-BASIS-PRED PAIR-PRED))

	; ------------------------------------------------------------

	(define (subtotal-trim LEFT-CUT RIGHT-CUT PAIR-CUT)
		(define sup-obj (add-support-api stars-obj))

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

		(trim-generic sup-obj left-basis-pred right-basis-pred pair-pred)
	)

	; ------------------------------------------------------------

	(define (support-trim LEFT-CUT RIGHT-CUT PAIR-CUT)
		(define sup-obj (add-support-api stars-obj))

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

		(trim-generic sup-obj left-basis-pred right-basis-pred pair-pred)
	)

	; -------------
	(define (help)
		(format #t
			(string-append
"This is the `add-trimmer` object applied to the \"~A\" object.\n"
"It provides methods to remove (delete) rows, columns and matrix\n"
"entries that pass assorted predicates. It is generally used to remove\n"
"unwanted junk, garbage and noise from datasets. By removing, rather\n"
"than filtering, the resulting dataset is generally smaller, and\n"
"accessing matrix entries is generally faster. For more information, say\n"
"`,d add-trimmer` or `,describe add-trimmer` at the guile prompt,\n"
"or just use the 'describe method on this object. You can also get at\n"
"the base object with the 'base method: e.g. `((obj 'base) 'help)`.\n"
)
		(LLOBJ 'id)))

	(define (describe)
		(display (procedure-property add-trimmer 'documentation)))

	; -------------
	; Methods on this class.
	(lambda (message . args)
		(case message
			((generic-trim)     (apply generic-trim args))
			((subtotal-trim)    (apply subtotal-trim args))
			((support-trim)     (apply support-trim args))

			((help)             (help))
			((describe)         (describe))
			((obj)              "add-trimmer")
			((base)             LLOBJ)

			; Block anything that might have to be filtered.
			; For example: 'pair-freq which we don't, can't filter.
			; Or any of the various subtotals and marginals.
			(else               (apply LLOBJ (cons message args))))
	)
)

; ================= END OF FILE ===============
