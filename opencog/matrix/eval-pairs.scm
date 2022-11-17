;
; eval-pairs.scm
;
; Define an EvaluationLink matrix API object.
;
; Copyright (c) 2013, 2014, 2017, 2020 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The object below provides a matrix API to access pairs of Atoms, held
; in an EvaluationLink. This API unlocks a suite of tools for computing
; various properties of the matrix, including frequencies, marginal
; probabilities and assorted vector-space properties.  By "matrix" it
; is meant a rank-2 parse matrix, a matrix of (left,right) paris or
; (row, column) pairs. This is exactly the API needed to unlock the
; toolset in the `(use-modules (opencog matrix))` statistical analysis
; subsystem.
;
; To be explicit: a sparse matrix can be encoded in the AtomSpace as
;
;     EvaluationLink
;         PredicateNode "some named relationship"
;         ListLink
;             SomeAtom "Some thing on the left (row)"
;             OtherNode "some other atom on right (column)"
;
; Matrix entries N(x,y) are stored as counts (numbers) on the
; EvaluationLink, with the `x` being the left atom, and `y`
; being the right atom.
;
; Given the generic API, the matrx system will compute marginals N(x,*)
; and N(*,y), a grand total N(*,*) and then probabilities:
;     p(x,y) = N(x,y)/N(*,*)
; and from this, various other statistical quantities, such as mutual
; information.
;
; ---------------------------------------------------------------------

(define-public (make-evaluation-pair-api PRED-NODE LEFT-TYPE RIGHT-TYPE
               ANY-LEFT ANY-RIGHT ID NAME)
"
  make-evaluation-pair-api -- Pair access methods for EvaluationLinks.

  This implements a matrix object representing atom-pairs, when these
  are encoded in an EvaluationLink in the usual style.  An atom pair is
  represented as:

    EvaluationLink
       PredicateNode \"Some named relation\"
       ListLink
          SomeAtom \"some thing on left (in a row)\"
          OtherNode \"other thing on right (in a column)\"

  This Atom (the EvaluationLink) be used to record counts, frequencies,
  entropies, etc pertaining to this particular pair, simply by placing
  them, as values, on the EvaluationLink. The EvaluationLink serves as
  a well-known location in a sparse rank-2 matrix; arbitrary data can
  be placed there.

  The PRED-NODE should be the `PredicateNode` encoding the relation.
  The LEFT-TYPE should be the atom type of the row-atoms.
  The RIGHT-TYPE should be the atom type of the column-atoms.
    (Atom types are scheme symbols, for example 'ConceptNode.)

  The ID should be a short string that serves as a unique id for
  this particular matrix. It is used when constructing submatrixes
  and filtered variants of this matrix.

  The NAME should be a long string describing the contents of this
  matrix. It is used as a title when printing summary reports.

  The ANY-LEFT and ANY-RIGHT should be atoms where row and column
  marginals can be stored.  The `AnyNode` is convenient for this
  purpose. Thus, for example, marginal probabilities for rows of
  genomic data might be encoded as:

    EvaluationLink
       PredicateNode \"Some named relation\"
       ListLink
          AnyNode \"left-gene\"
          GeneNode \"SIRT1\"

  The corresponding usage for this object would then be:

    (make-evaluation-pair-api (Predicate \"Some named relation\")
       'GeneNode 'GeneNode (Any \"left-gene\") (Any \"right-gene\")
       \"gene-pairs\" \"Interacting pairs of genes\")
"
	(let ((all-pairs '()))

		; Get the observational count on ATOM.
		(define (get-count ATOM) (cog-count ATOM))
		(define (inc-count ATOM)
			(cog-inc-count! ATOM 1))
		(define (set-count ATOM CNT)
			(cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))

		(define (get-left-type) LEFT-TYPE)
		(define (get-right-type) RIGHT-TYPE)
		(define (get-pair-type) 'EvaluationLink)

		; Return the atom holding the count, if it exists, else
		; return nil.
		(define (get-pair L-ATOM R-ATOM)
			(define maybe-list (cog-link 'ListLink L-ATOM R-ATOM))
			(if (null? maybe-list) '()
				(cog-link 'EvaluationLink PRED-NODE maybe-list)))

		; Create an atom to hold the count (if it doesn't exist already).
		(define (make-pair L-ATOM R-ATOM)
			(EvaluationLink PRED-NODE (List L-ATOM R-ATOM)))

		; Return the left member of the pair. Given the pair-atom,
		; locate the left-side atom.
		(define (get-left-element PAIR)
			(gadr PAIR))
		(define (get-right-element PAIR)
			(gddr PAIR))

		; Return the raw observational count on PAIR. If the counter for
		; PAIR does not exist (was not observed), then return 0.
		(define (get-pair-count L-ATOM R-ATOM)
			(define pr (get-pair L-ATOM R-ATOM))
			(if (null? pr) 0 (get-count pr)))

		; Caution: this unconditionally creates the wildcard pair!
		(define (get-left-wildcard WORD)
			(make-pair ANY-LEFT WORD))

		; Caution: this unconditionally creates the wildcard pair!
		(define (get-right-wildcard WORD)
			(make-pair WORD ANY-RIGHT))

		(define (get-wild-wild)
			(make-pair ANY-LEFT ANY-RIGHT))

		; get-all-pairs - return a list holding all of the observed
		; pairs.  Caution: this can be obscenely long!
		(define (do-get-all-pairs)
			; The list of pairs is mostly just the incoming set of the
			; predicate node. However, this does include some junk, so
			; filter out all rows and columns of the incorrect type.
		   (filter!
				(lambda (pair)
					(and
						(equal? LEFT-TYPE (cog-type (gadr pair)))
						(equal? RIGHT-TYPE (cog-type (gddr pair)))))
				(cog-incoming-by-type PRED-NODE 'EvaluationLink)))

		(define (get-all-pairs)
			(if (null? all-pairs) (set! all-pairs (do-get-all-pairs)))
			all-pairs)

		; fetch-all-pairs -- fetch all counts for atom pairs
		; from the currently-open database.
		(define (fetch-all-pairs)
			(define elapsed-secs (make-elapsed-secs))
			(fetch-incoming-set PRED-NODE)
			(format #t "Elapsed time to load pairs: ~A secs\n"
				(elapsed-secs))
		)

		; Delete the pairs from the atomspace AND the database.
		; But only those that are currently in the atomspace are
		; deleted; if any are hiding in the database, they will not be
		; touched.
		(define (delete-all-pairs)
			(define elapsed-secs (make-elapsed-secs))
			(for-each (lambda (PAIR) (cog-delete-recursive! (gdr PAIR)))
				(cog-incoming-set PRED-NODE))
			(cog-delete! PRED-NODE)
			(cog-delete! ANY-LEFT)
			(cog-delete! ANY-RIGHT)
			(format #t "Elapsed time to delete pairs: ~A secs\n"
				(elapsed-secs))
		)

      ;-------------------------------------------

		(define (help)
			(format #t
				(string-append
"This is the `make-evaluation-pair-api` object. It provides a matrix API\n"
"for the EvaluationLinks of the following form:\n"
"\n"
"     EvaluationLink\n"
"         PredicateNode \"~A\"\n"
"         ListLink\n"
"             ~A ...\n"
"             ~A ...\n"
"\n"
"All of the core matrix API methods are provided; these include:\n"
"\n"
"    name             The string name of the object\n"
"    id               The string ID of the object\n"
"    left-type        The type of the row Atoms\n"
"    right-type       The type of the column Atoms\n"
"    pair-type        Returns 'EvalutionLink\n"
"    pair-count L R   Returns the count on Evaluation Pred List L R\n"
"    get-pair L R     Returns Evaluation Pred List L R, if it exists, else null\n"
"    get-count E      Returns the count on E (an EvaluationLink)\n"
"    inc-count E      Atomic increment of the count on E by one\n"
"    set-count E N    Sets the count on E to N\n"
"    make-pair L R    Unconditionally make Evaluation Pred List L R\n"
"    left-element E   Return the row Atom of the EvaluationLink E\n"
"    right-element E  Return the column Atom of the EvaluationLink E\n"
"    left-wildcard R  Return Evaluation Pred List ANY R\n"
"    right-wildcard L Return Evaluation Pred List L ANY\n"
"    wild-wild        Return Evaluation Pred List ANY ANY\n"
"    all-pairs        Return a list of all non-zero entries in the matrix\n"
"    fetch-pairs      Fetch all matrix entries from currently-open database\n"
"    delete-pairs     Delete all pairs for this matrix in the AtomSpace\n"
"    help             Print this message\n"
"    describe         Print documentation for make-evaluation-pair-api\n"
)
				(cog-name PRED-NODE) LEFT-TYPE RIGHT-TYPE)
			*unspecified*)

		(define (describe)
			(display (procedure-property make-evaluation-pair-api 'documentation)))

      ;-------------------------------------------

		; Methods on the object.
		(lambda (message . args)
			(apply (case message
					((name)             (lambda () NAME))
					((id)               (lambda () ID))
					((left-type)        get-left-type)
					((right-type)       get-right-type)
					((pair-type)        get-pair-type)
					((pair-count)       get-pair-count)
					((get-pair)         get-pair)
					((get-count)        get-count)
					((inc-count)        inc-count)
					((set-count)        set-count)
					((make-pair)        make-pair)
					((left-element)     get-left-element)
					((right-element)    get-right-element)
					((left-wildcard)    get-left-wildcard)
					((right-wildcard)   get-right-wildcard)
					((wild-wild)        get-wild-wild)
					((get-all-elts)     get-all-pairs)
					((fetch-pairs)      fetch-all-pairs)
					((delete-pairs)     delete-all-pairs)
					((provides)         (lambda (symb) #f))
					((filters?)         (lambda () #f))
					((help)             help)
					((describe)         describe)
					(else (error "Bad method call on evaluation-api:" message)))
				args)))
)

; ---------------------------------------------------------------------
