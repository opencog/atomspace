;
; basic-api.scm
;
; unit-test the basic parts of the matrix object API.
;
; Copyright (c) 2017, 2018 Linas Vepstas
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog persist) (opencog matrix))

; This is cut-n-pasted from `opencog/matrix/object-api.scm`
; where it's used as the example.
(define (make-basic-api)

	; Return the atom-type of the matrix rows, i.e. the type of
	; the left side of the (row, column) pairs.
	(define (get-left-type) 'WordNode)

	; Return the atom-type of the matrix columns, i.e. the type of
	; the right side of the (row, column) pairs.
	(define (get-right-type) 'WordNode)

	; Return the type of the link that holds the pair.  In this
	; example, each pair will be held in the form
	;  (Evaluation (Predicate "foo") (List (Word "row") (Word "col")))
	(define (get-pair-type) 'EvaluationLink)

	; Return the atom for a matrix (row,column) pair, if it exists,
	; else return nil. In this example, the matrix is defined by an
	; EvaluationLink holding the ListLink. This atom is where all
	; values associated with this matrix are held.  This includes not
	; only the count (the number of observations of the pair) but also
	; any derived values, such as frequency, mutual information, and
	; so on. Users are free to (are encouraged to) use this atom to
	; attach additional information and statistics.
	;
	(define (get-pair L-ATOM R-ATOM)
		(define maybe-list (cog-link 'ListLink L-ATOM R-ATOM))
		(if (null? maybe-list) '()
			(cog-link 'EvaluationLink (Predicate "foo") maybe-list)))

	; Return the observed count for the pair PAIR.
	(define (get-count PAIR)
		(cog-value-ref (cog-value PAIR (Predicate "counter")) 2))

	; Return the observed count for the pair (L-ATOM, R-ATOM), if it
	; exists, else return zero.
	(define (get-pair-count L-ATOM R-ATOM)
		(define stats-atom (get-pair L-ATOM R-ATOM))
		(if (null? stats-atom) 0 (get-count stats-atom)))

	; Return the atom holding the count, creating it if it does
	; not yet exist.  Returns the same structure as the 'item-pair
	; method (the get-pair function, above).
	(define (make-pair L-ATOM R-ATOM)
		(EvaluationLink (Predicate "foo") (List L-ATOM R-ATOM)))

	; Return the left member of the pair. Given the pair-atom,
	; locate the left-side atom.
	(define (get-left-element PAIR)
		(gadr PAIR))

	(define (get-right-element PAIR)
		(gddr PAIR))

	; Return an atom to which column subtotals can be attached,
	; such as, for example, the subtotal `N(*,y)`. Thus, `y`
	; denotes a column, and the star is on the left (the star
	; ranging over all rows).
	(define (get-left-wildcard ITEM)
		(EvaluationLink (Predicate "foo")
			(ListLink (AnyNode "left-wild") ITEM)))

	; Return an atom to which row subtotals can be attached,
	; such as, for example, the subtotal `N(x,*)`. Thus, `x`
	; denotes a row, and the star is on the right (the star
	; ranging over all columns).
	(define (get-right-wildcard ITEM)
		(EvaluationLink (Predicate "foo")
			(ListLink ITEM (AnyNode "right-wild"))))

	; Return an atom to which matrix totals can be attached,
	; such as, for example, the total `N(*,*)`. This can be any
	; atom, but must be unique to the specific matrix. It's
	; convenient to use the same style as the subtotals.
	(define (get-wild-wild)
		(EvaluationLink (Predicate "foo")
			(ListLink (AnyNode "left-wild") (AnyNode "right-wild"))))

	; Retrieve, from storage, the entire matrix, including the
	; subtotal and total anchor atoms.  In this example, its enough
	; to get the incoming set of (Predicate "foo"), but this need
	; not generally be the case.
	(define (fetch-all-pairs)
		(fetch-incoming-by-type (Predicate "foo") 'EvaluationLink))

	; Methods on the class. To call these, quote the method name.
	; Example: (OBJ 'left-wildcard WORD) calls the
	; get-left-wildcard function, passing WORD as the argument.
	;
	; The name is a string printed at the top of generated reports.
	; The id is a short string used to create unique filter ids and names.
	(lambda (message . args)
		(apply (case message
				((name) (lambda () "The Unit Test Demo Object"))
				((id)	(lambda () "unit-test-demo"))
				((left-type) get-left-type)
				((right-type) get-right-type)
				((pair-type) get-pair-type)
				((pair-count) get-pair-count)
				((get-pair) get-pair)
				((get-count) get-count)
				((make-pair) make-pair)
				((left-element) get-left-element)
				((right-element) get-right-element)
				((left-wildcard) get-left-wildcard)
				((right-wildcard) get-right-wildcard)
				((wild-wild) get-wild-wild)
				((fetch-pairs) fetch-all-pairs)
				((provides) (lambda (symb) #f))
				((filters?) (lambda () #f))
				(else (error "Bad method call on low-level API")))
			args)))

; ---------------------------------------------------------------------

(define bapi (make-basic-api))
(define sapi (add-pair-stars bapi))
(define fapi (add-pair-freq-api sapi))

(define papi (add-support-api sapi))
(define pcmp (add-support-compute bapi))

(define cent (make-central-compute bapi))
(define rapi (add-report-api bapi))

(define cosi (add-similarity-compute bapi))

(define dapi (add-dynamic-stars bapi))
(define dosi (add-similarity-compute dapi))

(define suby (add-tuple-math bapi -))
(define norm (add-support-compute suby))

(define tcom (add-transpose-compute bapi))
(define tapi (add-transpose-api bapi))

(define symc (add-symmetric-mi-compute bapi))

(define prod-t (add-support-compute (add-tuple-math sapi *)))
(define prod-f (add-support-compute (add-fast-math sapi *)))

*unspecified*
