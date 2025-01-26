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

; Stick some random numbers onto the raw data.

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
