;
; float-column-test.scm -- Verify that FloatColumn works.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "float-column-test")
(test-begin tname)

; ------------------------------------------------------------
; Serialize numbers. Trivial case.

(define num (NumberNode 1 2 3 4))
(define ncol (FloatColumn num))
(define nvec (cog-execute! ncol))
(format #t "number vect: ~A\n" nvec)
(test-assert "number vect" (equal? nvec (FloatValue 1 2 3 4)))

; ------------------------------------------------------------
; Serialize numbers, in list form.

(define numli (List
	(NumberNode 1)
	(NumberNode 2)
	(NumberNode 3)
	(NumberNode 4)))
(define nlicol (FloatColumn numli))
(define nlivec (cog-execute! nlicol))
(format #t "number list vect: ~A\n" nlivec)
(test-assert "number list vect" (equal? nlivec (FloatValue 1 2 3 4)))

; ------------------------------------------------------------
; Serialize numbers, in direct form.

(define numset (list
	(NumberNode 1)
	(NumberNode 2)
	(NumberNode 3)
	(NumberNode 4)))
(define nsetcol (FloatColumn numset))
(define nsetvec (cog-execute! nsetcol))
(format #t "number set vect: ~A\n" nsetvec)
(test-assert "number set vect" (equal? nlivec (FloatValue 1 2 3 4)))

; ------------------------------------------------------------
; Serialize LinkValue lists.

(define floli (LinkValue
	(FloatValue 1)
	(FloatValue 2)
	(FloatValue 3)
	(FloatValue 4)))
(cog-set-value! (Anchor "heavy") (Predicate "weight") floli)

(define flocol
	(FloatColumn (ValueOf (Anchor "heavy") (Predicate "weight"))))

(define flovec (cog-execute! flocol))
(format #t "Float vect: ~A\n" flovec)
(test-assert "float list vect" (equal? flovec (FloatValue 1 2 3 4)))

; ------------------------------------------------------------
; Complicated case, attempts to mode real world case.

; Data
(Edge (Predicate "word-pair") (List (Item "Paul") (Item "bit")))
(Edge (Predicate "word-pair") (List (Item "bit") (Item "the")))
(Edge (Predicate "word-pair") (List (Item "the") (Item "dog")))
(Edge (Predicate "word-pair") (List (Item "dog") (Item "in")))
(Edge (Predicate "word-pair") (List (Item "in") (Item "the")))
(Edge (Predicate "word-pair") (List (Item "the") (Item "leg")))
(Edge (Predicate "word-pair") (List (Item "leg") (Item "and")))
(Edge (Predicate "word-pair") (List (Item "and") (Item "it")))
(Edge (Predicate "word-pair") (List (Item "it") (Item "hurt")))
(Edge (Predicate "word-pair") (List (Item "hurt") (Item "a")))
(Edge (Predicate "word-pair") (List (Item "a") (Item "lot")))
(Edge (Predicate "word-pair") (List (Item "lot") (Item ".")))

; -------
; Jam that data into one big LinkValue list.
(define mtxpr
	(Query (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
		(Present
			(Edge (Predicate "word-pair")
				(List (Variable "$left-word") (Variable "$right-word"))))
		(Edge (Predicate "word-pair")
			(List (Variable "$left-word") (Variable "$right-word")))))

(cog-execute! mtxpr)

; -------
; Stick some random numbers onto the raw data. These will be our
; "weights"
(cog-set-value!
	(Anchor "heavy") (Predicate "randgen 1") (RandomStream 1))

(define tag-pairs-randomly
	(Filter
		(Rule
			(Variable "$edge")
			(Variable "$edge")
			(SetValue (Variable "$edge") (Predicate "weight")
				(StreamValueOf (Anchor "heavy") (Predicate "randgen 1"))))
		(ValueOf mtxpr mtxpr)))

(cog-execute! tag-pairs-randomly)
; -------
; Go grab numbers off the data, and convert it to a column

(define datacol
	(FloatColumn
		(Filter
			(Rule
				(Variable "$edge")
				(Variable "$edge")
				(FloatValueOf (Variable "$edge") (Predicate "weight")))
		(ValueOf mtxpr mtxpr))))

(define datavec (cog-execute! datacol))
(format #t "Data vect: ~A\n" datavec)

; Twelve data items, so twelve numbers
(test-assert "data list length" (equal? 12
	 (length (cog-value->list datavec))))

; ------------------------------------------------------------
; Stick a vector of "statistical values" onto the raw data.
; The square and cube of the weights, for this example.
; Then rip these out one column at a time,

(define edge-weight
	(FloatValueOf (Variable "$edge") (Predicate "weight")))

(define tag-pairs-w-stats
	(Filter
		(Rule
			(Variable "$edge")
			(Variable "$edge")
			(SetValue (Variable "$edge") (Predicate "stats")
				(FloatColumn
					edge-weight
					(Times edge-weight edge-weight)
					(Times edge-weight edge-weight edge-weight))))
		(ValueOf mtxpr mtxpr)))

(cog-execute! tag-pairs-w-stats)

; -------
; Go grab the third number from the stats vec, and convert it to a column

(define (grab-col COLNO)
	(FloatColumn
		(Filter
			(Rule
				(Variable "$edge")
				(Variable "$edge")
				(ElementOf (Number COLNO)
					(FloatValueOf (Variable "$edge") (Predicate "stats"))))
		(ValueOf mtxpr mtxpr))))

(define cubecol (grab-col 2))
(define cubevec (cog-execute! cubecol))
(format #t "Cube vect: ~A\n" cubevec)

; Twelve data items, so twelve numbers
(test-assert "cube list length" (equal? 12
	 (length (cog-value->list cubevec))))

(define squarecol (grab-col 1))
(define squarevec (cog-execute! squarecol))
(format #t "Square vect: ~A\n" squarevec)

; Twelve data items, so twelve numbers
(test-assert "square list length" (equal? 12
	 (length (cog-value->list squarevec))))

(define origcol (grab-col 0))
(define origvec (cog-execute! origcol))
(format #t "Orig vect: ~A\n" origvec)

; Twelve data items, so twelve numbers
(test-assert "orig list length" (equal? 12
	 (length (cog-value->list origvec))))

; The first col should be equal to the original weight data.
(test-assert "orig and data equal" (equal? datavec origvec))

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
