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

	; Return the type of the pair itself.  In this example, each pair
	; will be of the form (ListLink (Word "row") (Word "col"))
	(define (get-pair-type) 'ListLink)

	; Return the atom for a matrix (row,column) pair, if it exists,
	; else return nil. In this example, the matrix is defined by an
	; EvaluationLink holding the ListLink. This atom is where all
	; values associated with this matrix are held.  This includes not
	; only the count (the number of observations of the pair) but also
	; any dervides values, such as frequency, mutual information, and
	; so on. Users are free to (are encouraged to) use this atom to
	; attach additional information and statistics.
	;
	; The PAIR atom must be of 'pair-type, that is, a ListLink in this
	; example.  Note: the cog-link function does NOT create the atom,
	; if it does not already exist!
	(define (get-pair PAIR)
		(cog-link 'EvaluationLink (Predicate "foo") PAIR))

	; Return the observed count for PAIR, if it exists, else
	; return zero. The PAIR atom must be of type 'pair-type;
	; that is, a ListLink in this example.
	(define (get-pair-count PAIR)
		(define stats-atom (get-pair PAIR))
		(if (null? stats-atom) 0
			(cog-value-ref
				 (cog-value stats-atom (Predicate "counter")) 2)))

	; Return the atom holding the count, creating it if it does
	; not yet exist.  Returns the same structure as the 'item-pair
	; method (the get-pair function, above).
	(define (make-pair PAIR)
		(EvaluationLink (Predicate "foo") PAIR))

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
				((item-pair) get-pair)
				((make-pair) make-pair)
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
(define capi (add-pair-count-api sapi))
(define fapi (add-pair-freq-api capi))

(define rapi (add-report-api bapi))

*unspecified*
