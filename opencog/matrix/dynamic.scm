;
; dynamic.scm
;
; Matrix algorithms that do not require that the entire dataset
; be loaded into RAM all at the same time.  Instead, portions are
; "dynamically" loaded, as needed, from the database.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (opencog persist))

; ---------------------------------------------------------------------
; TODO: this class sort-of works for the simplest case, but is
; incomplete. Support for duals is missing. It also won't work
; for pairs that have a non-trivial structure.
;
; XXX the fetching of the basis should be moved to the base object API.

(define-public (add-dynamic-stars LLOBJ)
"
  add-dynamic-stars LLOBJ - Extend LLOBJ with row and column access
  methods (aka wildcard methods), specifically, to get all non-zero
  elements in a given row or column from the database backend.

  Similar to the (add-pair-stars LLOBJ) class, except that this
  attempts to work without having to load all pairs into RAM at the
  same time; instead, pairs are fetched from the database dynamically,
  on demand.  This is attempting to address the problem where not all
  of the matrix can fit into RAM at the same time.  If all of the
  matrix can fit, then (add-pair-stars LLOBJ) is more efficient.

  This overloads the standard add-pair-stars methods, so that the
  fetching occurs automatically and transparently to the user.

  The supported methods are:
  'left-basis - Return all items (atoms) that might be used to index
      a row in the matrix.  This may return more items than there are
      rows; no check is performed for empty or invalid rows. However,
      all valid rows will appear in the set: the returned set is a
      superset.  All of the elements of this set will be atoms of type
      (LLOBJ 'left-type).

  'right-basis - Likewise, but for columns.

  'left-stars COL - Returns pairs (*, COL), same as documented in the
  pair-stars API.

  'right-stars ROW - Likewise, but returns the set (ROW, *).

  'left-release COL - Remove pairs (*, COL) from the atomspace. This
  intended to be used to minimize RAM usage when working with a large
  database.  The atoms are NOT removed from the database; only from
  the atomspace. If the atoms are in use (have a non-empty incoming
  set) they are not removed.

  'right-release ROW - same but for pairs in ROW.
"
	(let ((stars-obj (add-pair-stars LLOBJ))
			(l-basis '())
			(r-basis '())
			(done-already? (make-once-predicate))
			(pair-type (LLOBJ 'pair-type))
		)

		; Retrieve all atoms of TYPE from the database
		(define (get-atoms TYPE)
			(load-atoms-of-type TYPE)
			(cog-get-atoms  TYPE))

		; Return a list of all items that might be rows.
		(define (get-left-basis)
			(if (null? l-basis)
				(set! l-basis (get-atoms (LLOBJ 'left-type))))
			l-basis)

		; Return a list of all items that might be columns.
		(define (get-right-basis)
			(if (null? r-basis)
				(set! r-basis (get-atoms (LLOBJ 'right-type))))
			r-basis)

		; Fetch the incoming set for ITEM, but only if we haven't
		; already done so. XXX FIXME: this only works if ITEM is
		; immediately under 'pair-type. If its deeper, its broken.
		(define (get-incoming ITEM)
			(if (not (done-already? ITEM))
				(fetch-incoming-by-type ITEM pair-type)))

		; Return a list matrix entries with ITEM on the right;
		; that is, a wild-card on the left. That is, the set of
		; entries { (*, ITEM) }.  Uses the stars-obj to filter
		; the valid pairs, after fetching them from the database.
		(define (get-left-stars ITEM)
			(get-incoming ITEM)
			(stars-obj 'left-stars ITEM))

		(define (get-right-stars ITEM)
			(get-incoming ITEM)
			(stars-obj 'right-stars ITEM))

		; XXX TODO: implement left and right duals.

		;-------------------------------------------
		; Release (extract) row or column. No specific check is made
		; to really be sure that this is a part of the matrix; it's
		; assumed that the pair-type is enough to achieve this.
		(define (release-extract ITEM)
			(for-each cog-extract! (cog-incoming-by-type ITEM pair-type)))

		;-------------------------------------------
		; Explain what it is that I provide. This helps classes
		; above understand what it is that this class does.
		(define (provides meth)
			(case meth
				((left-basis)       get-left-basis)
				((right-basis)      get-right-basis)
				((left-stars)       get-left-stars)
				((right-stars)      get-right-stars)
				(else               (LLOBJ 'provides meth))))

		;-------------------------------------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((left-basis)     (get-left-basis))
				((right-basis)    (get-right-basis))
				((left-stars)     (apply get-left-stars args))
				((right-stars)    (apply get-right-stars args))
				((left-release)   (apply release-extract args))
				((right-release)  (apply release-extract args))
				((clobber)        (stars-obj 'clobber))
				((provides)       (apply provides args))
				(else             (apply LLOBJ (cons message args))))
		)))

; ---------------------------------------------------------------------
