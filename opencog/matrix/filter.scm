;
; filter.scm
;
; Define API's for filtering the matrixes, e.g. by removing rows,
; columns or individual entries with low counts.
;
; Copyright (c) 2017, 2019 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Large datasets are inherently likely to contain "noise" and spurious
; data that might be unwanted during data analysis. For example, the
; dataset might contain a large number of atoms that were observed only
; once or twice; these are likely to be junk and should be removed
; before data analysis begins.
;
; The code here provides this filtering ability. Several types of
; filters are provided:
; -- a knockout filter, that knocks out designated rows and columns.
; -- a min-count filter, that knocks out rows and columns whose
;    marginal counts are below a minimum, as well as individual matrix
;    entries that are below a per-entry minimum.
; -- a generic callback-defined filter, that knocks out rows, columns
;    and individual entries based on callback predicates.
;
; Note that these filters are all "on demand": they do NOT copy the
; dataset and then compute a smaller version of it.  Instead, they
; overload the "star" API, altering the methods used to fetch rows,
; columns and individual entries.  Since all the other matrix access
; routines use the "star" API to gain access to the matrix and it's
; marginals, this works!
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

; ---------------------------------------------------------------------

(define-public (add-generic-filter LLOBJ
	LEFT-BASIS-PRED RIGHT-BASIS-PRED PAIR-PRED
   ID-STR RENAME)
"
  add-generic-filter LLOBJ - Modify LLOBJ so that only the columns and
  rows and individual entries that satisfy the predicates are retained.

  This provides the same methods as the `add-pair-stars` object; it is
  a replacement of that object, for all practical purposes. Thus, given
  some matrix, this object provides a way of accessing a smaller,
  lower-dimensional version of that matrix, by knocking out rows,
  columns and individual entries.

  Note, however, that the full-sized matrix will typically have
  marginals associated with it (such as totals over columns and rows)
  and that those marginals will typically become invalid for the
  smaller matrix (because totals over columns and rows for the smaller
  matrix are necessarily different). This filter does NOT attempt to
  recompute those marginals!  It is up  to the user to track these!
  The ID-STR can be used to give this submatrix a unique name, and
  therefore can be used as a tag, when recomputing marginals for the
  smaller matrix.

  The LEFT-BASIS-PRED and RIGHT-BASIS-PRED should be functions that
  accept atoms in the left and right basis, and return #t if they
  should be kept. If these predicates return #f, that row or column
  will not appear in the filtered matrix.

  The PAIR-PRED should be a function to that accepts individual matrix
  entries. It is applied whenever the 'get-pair or 'get-count methods
  are invoked.  Like the others, it should return #t to keep the pair.

  The ID-STR should be a string; it is appended to the dataset name and
  id, so that unique identifier names can be constructed for each
  filtered dataset.

  The RENAME argument should be #f or #t, it is used to determine how
  other API's generate predicate keys to obtain values.

  The filtered matrix may contain empty rows or columns! This can
  occur when the PAIR-PRED removes everything in a row or column,
  but the other predicates do not. These can be removed by applying
  the `add-zero-filter` object afterwards.
"
	(let ((stars-obj (add-pair-stars LLOBJ))
			(l-basis #f)
			(r-basis #f)
			(l-size 0)
			(r-size 0)
			(all-elts #f)
		)

		; Cache the result of filtering basis elements
		(define cache-left-pred (make-afunc-cache LEFT-BASIS-PRED))
		(define cache-right-pred (make-afunc-cache RIGHT-BASIS-PRED))

		; ---------------
		; Filter out rows and columns that pass the left and right
		; predicates.
		(define (do-left-basis)
			(filter cache-left-pred (stars-obj 'left-basis)))

		(define (do-right-basis)
			(filter cache-right-pred (stars-obj 'right-basis)))

		; ---------------
		; Use the cached value, if its there.
		(define (get-left-basis)
			(if (not l-basis) (set! l-basis (do-left-basis)))
			l-basis)

		(define (get-right-basis)
			(if (not r-basis) (set! r-basis (do-right-basis)))
			r-basis)

		(define (get-left-size)
			(if (eq? 0 l-size) (set! l-size (length (get-left-basis))))
			l-size)

		(define (get-right-size)
			(if (eq? 0 r-size) (set! r-size (length (get-right-basis))))
			r-size)

		; Invalidate all the caches, and pass the clober down to the
		; stars.
		(define (clobber)
			(stars-obj 'clobber)
			(set! l-basis #f)
			(set! r-basis #f)
			(set! l-size 0)
			(set! r-size 0)
			(set! all-elts #f)
		)

		; ---------------
		; Return only those duals that pass the cutoffs.
		;
		(define (do-left-duals RITEM)
			; Get all the left-elements corresponding to RITEM
			(filter
				(lambda (LITEM)
					(and
						(cache-left-pred LITEM)
						(PAIR-PRED (LLOBJ 'get-pair LITEM RITEM))))
				(stars-obj 'left-duals RITEM)))

		(define (do-right-duals LITEM)
			(filter
				(lambda (RITEM)
					(and
						(cache-right-pred RITEM)
						(PAIR-PRED (LLOBJ 'get-pair LITEM RITEM))))
				(stars-obj 'right-duals LITEM)))

		; Cache the results above, so that we don't recompute over and over.
		(define cache-left-duals (make-afunc-cache do-left-duals))
		(define cache-right-duals (make-afunc-cache do-right-duals))

		; ---------------
		; Return only those stars that pass the cutoff.
		;
		(define (do-left-stars RITEM)
			; Apply the pair-pred cutoff
			(filter PAIR-PRED
				(map
					; Convert all left-right pairs into real pairs
					(lambda (LBASE) (LLOBJ 'get-pair LBASE RITEM))
					; Get all the left-elements corresponding to RITEM
					(filter cache-left-pred (stars-obj 'left-duals RITEM)))))

		(define (do-right-stars LITEM)
			(filter PAIR-PRED
				(map
					(lambda (RBASE) (LLOBJ 'get-pair LITEM RBASE))
					(filter cache-right-pred (stars-obj 'right-duals LITEM)))))

		; Cache the results above, so that we don't recompute over and over.
		(define cache-left-stars (make-afunc-cache do-left-stars))
		(define cache-right-stars (make-afunc-cache do-right-stars))

		; ---------------
		; Apply the pair-cut to each pair.
		(define (get-item-pair L-ATOM R-ATOM)
			(define PAIR (LLOBJ 'get-pair L-ATOM R-ATOM))
			(if (PAIR-PRED PAIR) PAIR '()))

		(define (get-count PAIR)
			(if (PAIR-PRED PAIR) (LLOBJ 'get-count PAIR) 0))

		(define (get-pair-count L-ATOM R-ATOM)
			(define stats-atom (get-item-pair L-ATOM R-ATOM))
			(if (null? stats-atom) 0 (LLOBJ 'get-count stats-atom)))

		; ---------------
		; Exhaustive loop over pairs. For each left element, get all
		; right stars; the right-stars already have the pair pred
		; applied. The atom-set is used to avoid duplicates, but in
		; the normal scheme of things, there should be no duplicates
		; so that check is mostly superfluous.
		(define (do-get-all-elts)
			(define aset (make-atom-set))
			(for-each
				(lambda (litem) (for-each aset (cache-right-stars litem)))
				(get-left-basis))
			(aset #f)
		)
		(define (get-all-elts)
			(if (not all-elts) (set! all-elts (do-get-all-elts)))
			all-elts)

		; ---------------
		(define (get-name)
			(string-append (LLOBJ 'name) " " ID-STR))
		(define (get-id)
			(string-append (LLOBJ 'id) " " ID-STR))

		; ---------------
		(define (help)
			(format #t
				(string-append
"This is the `add-generic-filter` object applied to \"~A\" to\n"
"create \"~A\", with filtering ~A. The filter object knocks\n"
"out rows, columns and individual entries from the larger matrix\n"
"object. It effectively creates a smaller matrix from a bigger one,\n"
"by overloading the default access functions.\n"
)
				(LLOBJ 'id) ID-STR (if RENAME "enabled" "disabled")))

		(define (describe)
			(display (procedure-property add-generic-filter 'documentation)))

		; ---------------
		; Return a pointer to each method that this class overloads.
		(define (provides meth)
			(case meth
				((left-basis)       get-left-basis)
				((right-basis)      get-right-basis)
				((left-basis-size)  get-left-size)
				((right-basis-size) get-right-size)
				((left-stars)       cache-left-stars)
				((right-stars)      cache-right-stars)
				((left-duals)       cache-left-duals)
				((right-duals)      cache-right-duals)
				((get-all-elts)     get-all-elts)
				((clobber)          clobber)
				(else               (LLOBJ 'provides meth))))

		; -------------
		; Methods on this class.
		(lambda (message . args)
			(case message
				((name)             (get-name))
				((id)               (get-id))
				((left-basis)       (get-left-basis))
				((right-basis)      (get-right-basis))
				((left-basis-size)  (get-left-size))
				((right-basis-size) (get-right-size))
				((left-stars)       (apply cache-left-stars args))
				((right-stars)      (apply cache-right-stars args))
				((left-duals)       (apply cache-left-duals args))
				((right-duals)      (apply cache-right-duals args))
				((get-pair)         (apply get-item-pair args))
				((get-count)        (apply get-count args))
				((get-pair-count)   (apply get-pair-count args))
				((get-all-elts)     (get-all-elts))
				((clobber)          (clobber))
				((provides)         (apply provides args))
				((filters?)         RENAME)

				((help)             (help))
				((describe)         (describe))
				((obj)              "add-generic-filter")
				((base)             LLOBJ)

				; Pass through some selected methods
				((left-type)        (apply LLOBJ (cons message args)))
				((right-type)       (apply LLOBJ (cons message args)))
				((pair-type)        (apply LLOBJ (cons message args)))
				((left-wildcard)    (apply LLOBJ (cons message args)))
				((right-wildcard)   (apply LLOBJ (cons message args)))
				((wild-wild)        (apply LLOBJ (cons message args)))
				; Block anything that might have to be filtered.
				; For example: 'pair-freq which we don't, can't filter.
				; Or any of the various subtotals and marginals.
				(else               (throw 'bad-use 'add-generic-filter
					(format #f "Sorry, method ~A not available on filter!"
						message))))
		)))

; ---------------------------------------------------------------------

(define-public (add-subtotal-filter LLOBJ LEFT-CUT RIGHT-CUT PAIR-CUT RENAME)
"
  add-subtotal-filter LLOBJ - Modify LLOBJ so that any columns and
  rows with counts less than LEFT-CUT and RIGHT-CUT are removed, and that
  individual entries with counts less than PAIR-CUT are removed. This
  provides an API compatible with the star-object API; i.e. it provides
  the same row and column addressability that star-object does, but
  just returns fewer rows, columns and individual entries.

  The filtering is done 'on demand', on a row-by-row, column-by-column
  basis.  Computations of the left and right stars are cached, so that
  they are not recomputed for each request.

  Note that by removing rows and columns, the frequencies that were
  computed for the entire matrix will no longer sum to 1.0 for the
  filtered submatrix.  Likewise, row and column subtotals, and any
  marginals will no longer sum or behave as in the whole dataset.
  If accurate values for these are needed, then they would need to
  be recomputed for the reduced matrix. The 'filters? method, and
  the RENAME argument provide a way for dealing with this.

  If the RENAME argument is #t, then the other various API's will use
  a special key name, created from the 'id of this filter, to access
  frequencies and marginals. This allows filtered frequencies and
  marginals to be stored within the same matrix.  If the RENAME
  argument is #f, then all access to the frequencies and marginals
  will be through the primary, main predicate keys.

  Thus, set RENAME to #f if you just want to cut down on the number of
  rows and columns, but otherwise use the normal data.  But if you need
  to recompute new values and marginals for the filtered matrix, then
  set RENAME to #t.

  Some terminology: Let N(x,y) be the observed count for the pair (x,y).
  Let N(*,y) be the column subtotals, AKA the left-subtotals.
  Let N(x,*) be the row subtotals, AKA the right subtotals.

  This object removes all columns where  N(*,y) <= RIGHT-CUT and all
  rows where N(x,*) <= LEFT-CUT.  Pairs are not reported in the
  'left-stars and 'right-stars methods when N(x,y) <= PAIR-CUT.
  Likewise, duals are not reported when the corresponding pair fails
  the PAIR-CUT.

  The net effect of the cuts is that when LEFT-CUT is increased, the
  left-dimension of the dataset drops; likewise on the right.
"
	(define stars-obj (add-pair-stars LLOBJ))
	(define sup-obj (add-support-api stars-obj))

	; ---------------
	; Filter out rows and columns that are below-count.
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

	(define id-str
		(format #f "subtotal-cut-~D-~D-~D"
			LEFT-CUT RIGHT-CUT PAIR-CUT))

	; ---------------
	; Filtering will fail, if the support has not been computed yet.
	; Check this now, and throw an error in this situation. Accessing
	; the total will cause an error to be throw, with a helpful message.
	(sup-obj 'total-support-left)

	; ---------------
	(add-generic-filter stars-obj
		left-basis-pred right-basis-pred pair-pred id-str RENAME)
)

; ---------------------------------------------------------------------

(define-public (add-fmi-filter LLOBJ FMI-CUT RENAME)
"
  add-mi-filter LLOBJ - Modify LLOBJ so that any pairs with a
  fraction MI score less than FMI-CUT are removed. This provides
  an API compatible with the star-object API; i.e. it provides the
  same row and column addressability that star-object does, but just
  returns fewer individual entries.

  The filtering is done 'on demand', on a row-by-row, column-by-column
  basis.  Computations of the left and right stars are cached, so that
  they are not recomputed for each request.

  Note that by removing entries, the effective FMI, and the entropy
  subtotals will no longer obey the relationships used to originally
  calculate them.  If accurate values are needed for the new, smaller
  matrix, then they would need to be recomputed. The 'filters? method,
  and the RENAME argument provide a way for dealing with this.

  If the RENAME argument is #t, then the other various API's will use
  a special key name, created from the 'id of this filter, to access
  frequencies and marginals. This allows filtered frequencies and
  marginals to be stored within the same matrix.  If the RENAME
  argument is #f, then all access to the frequencies and marginals
  will be through the primary, main predicate keys.

  Thus, set RENAME to #f if you just want to cut down on the number of
  entries, but otherwise use the normal data.  But if you need to
  recompute new values and marginals for the filtered matrix, then
  set RENAME to #t.

  Precise definition: Let FMI(x,y) be the (fractional) mutual
  information for the pair (x,y) (as returned by the 'pair-fmi method
  on the `add-pair-freq-api` object.)  Pairs are not reported in the
  'left-stars and 'right-stars methods when FMI(x,y) <= FMI-CUT.
  Likewise, duals are not reported when the corresponding pair fails
  the FMI-CUT.
"
	(define stars-obj (add-pair-stars LLOBJ))
	(define freq-obj (add-pair-freq-api stars-obj))

	(define (pair-pred PAIR) (< FMI-CUT (freq-obj 'pair-fmi PAIR)))

	(define id-str (format #f "fmi-cut-~6F" FMI-CUT))

	(define (true-pred ITEM) #t)

	(add-generic-filter stars-obj true-pred true-pred pair-pred
		id-str RENAME)
)

; ---------------------------------------------------------------------

(define-public (add-knockout-filter LLOBJ LEFT-KNOCKOUT RIGHT-KNOCKOUT RENAME)
"
  add-knockout-filter LLOBJ - Modify LLOBJ so that the explicitly
  indicated rows and columns are removed. The LEFT-KNOCKOUT and
  RIGHT-KNOCKOUT should be lists of left and right basis elements.
  This is the opposite of `add-keep-filter`.

  The RENAME argument should be #f or #t, it is used to determine how
  other API's generate predicate keys to obtain values.
"
	; ---------------
	; Filter out rows and columns in the knockout lists.
	;
	(define (left-basis-pred ITEM)
		(not (any
			(lambda (knockout) (equal? knockout ITEM))
			LEFT-KNOCKOUT)))

	(define (right-basis-pred ITEM)
		(not (any
			(lambda (knockout) (equal? knockout ITEM))
			RIGHT-KNOCKOUT)))

	(define (pair-pred PAIR) #t)

	(define id-str
		(format #f "knockout-~D-~D"
			(length LEFT-KNOCKOUT) (length RIGHT-KNOCKOUT)))

	; ---------------
	(add-generic-filter LLOBJ
		left-basis-pred right-basis-pred
		pair-pred id-str RENAME)
)

; ---------------------------------------------------------------------

(define-public (add-keep-filter LLOBJ LEFT-KEEP RIGHT-KEEP RENAME)
"
  add-keep-filter LLOBJ - Modify LLOBJ so that the explicitly
  indicated rows and columns are kept, and all others are removed.
  The LEFT-KEEP and RIGHT-KEEP should be lists of left and right
  basis elements. This is the 'opposite' of `add-knockout-filter`.

  The RENAME argument should be #f or #t, it is used to determine how
  other API's generate predicate keys to obtain values.
"
	; ---------------
	; Filter out rows and columns in the knockout lists.
	;
	(define (left-basis-pred ITEM)
		(any
			(lambda (keep) (equal? keep ITEM))
			LEFT-KEEP))

	(define (right-basis-pred ITEM)
		(any
			(lambda (keep) (equal? keep ITEM))
			RIGHT-KEEP))

	(define (pair-pred PAIR) #t)

	(define id-str
		(format #f "keep-~D-~D"
			(length LEFT-KEEP) (length RIGHT-KEEP)))

	; ---------------
	(add-generic-filter LLOBJ
		left-basis-pred right-basis-pred
		pair-pred id-str RENAME)
)

; ---------------------------------------------------------------------

(define-public (add-zero-filter LLOBJ RENAME)
"
  add-zero-filter LLOBJ RENAME - discard zero rows and columns.

  Given a matrix LLOBJ, this defines a new matrix that contains
  only those rows and columns with non-zero counts. This is useful
  when analyzing matrix-wide reports, where zero rows/columns can
  throw off averages.
"
	(define (left-basis-p ATOM)
		(any (lambda (PR) (< 0 (LLOBJ 'get-count PR)))
			(LLOBJ 'right-stars ATOM)))

	(define (right-basis-p ATOM)
		(any (lambda (PR) (< 0 (LLOBJ 'get-count PR)))
			(LLOBJ 'left-stars ATOM)))

	(define (pair-p PAIR) (< 0 (LLOBJ 'get-count PAIR)))

	(add-generic-filter LLOBJ left-basis-p right-basis-p pair-p
		"zero-filter" RENAME)
)
; ---------------------------------------------------------------------
