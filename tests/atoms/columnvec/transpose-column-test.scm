;
; transpose-column-test.scm -- Verify that TransposeColumn works.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "transpose-column-test")
(test-begin tname)

; ------------------------------------------------------------
(define numat
	(List
		(Number 1 2 3)
		(Number 4 5 6)))

(define nutria (cog-execute! (TransposeColumn numat)))

(format #t "Got nutria ~A\n" nutria)

(test-assert "nutria" (equal? nutria
	(LinkValue
		(FloatValue 1 4)
		(FloatValue 2 5)
		(FloatValue 3 6))))

(define possum (cog-execute! (TransposeColumn (TransposeColumn numat))))

(format #t "Got possum ~A\n" possum)

(test-assert "possum" (equal? possum
	(LinkValue
		(FloatValue 1 2 3)
		(FloatValue 4 5 6))))

; ------------------------------------------------------------

(define atomat
	(List
		(List (Item "a") (Item "b") (Item "c"))
		(List (Item "d") (Item "e") (Item "f"))))

(define atomatic (cog-execute! (TransposeColumn atomat)))

(format #t "Got atomatic ~A\n" atomatic)

(test-assert "atomatic" (equal? atomatic
	(LinkValue
		(LinkValue (Item "a") (Item "d"))
		(LinkValue (Item "b") (Item "e"))
		(LinkValue (Item "c") (Item "f")))))

(define vatom (cog-execute! (TransposeColumn (TransposeColumn atomat))))

(format #t "Got vatom ~A\n" vatom)

(test-assert "vatom" (equal? vatom
	(LinkValue
		(LinkValue (Item "a") (Item "b") (Item "c"))
		(LinkValue (Item "d") (Item "e") (Item "f")))))


; ------------------------------------------------------------
; Populate AtomSpace with "real world" data.

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

(define edge-pattern
	(Edge (Predicate "word-pair")
		(List (Variable "$left-word") (Variable "$right-word"))))
; -------
; Count occurances.
(define mtxpr
	(Query (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
		(Present edge-pattern)
		(IncrementValue (Variable "$left-word")
			(Predicate "counter") (NumberNode 1 0 -0.3))
		(IncrementValue (Variable "$right-word")
			(Predicate "counter") (NumberNode 0 1 -0.3))
		(IncrementValue edge-pattern
			(Predicate "counter") (NumberNode 0 0 1))))

(cog-execute! mtxpr)
(format #t "matrix is ~A\n" (cog-execute! (ValueOf mtxpr mtxpr)))

(define trannie (cog-execute!
	(TransposeColumn (ValueOf mtxpr mtxpr))))

(format #t "trannie is ~A\n" trannie)

(define cols (cog-value->list trannie))
(test-assert "expect three columns" (equal? 3 (length cols)))

(for-each
	(lambda (col)
		(test-assert "expect len=12"
			(equal? 12 (length (cog-value->list col)))))
	cols)

; -------
; Count and label.
(define labels
	(Query (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
		(Present edge-pattern)
		(SexprColumn (Variable "$left-word"))
		(IncrementValue (Variable "$left-word")
			(Predicate "counter") (NumberNode 1 0 -0.3))
		(SexprColumn (Variable "$right-word"))
		(IncrementValue (Variable "$right-word")
			(Predicate "counter") (NumberNode 0 1 -0.3))
		(IncrementValue edge-pattern
			(Predicate "counter") (NumberNode 0 0 1))
		;;; (SexprColumn edge-pattern)
	))

(cog-execute! labels)

(format #t "labels are ~A\n" (cog-execute! (ValueOf labels labels)))

(define arrows (cog-execute!
	(TransposeColumn (ValueOf labels labels))))

(format #t "arrows are ~A\n" arrows)

(define arco (cog-value->list arrows))
(test-assert "expect five columns" (equal? 5 (length arco)))

(for-each
	(lambda (col)
		(test-assert "expect len=12"
			(equal? 12 (length (cog-value->list col)))))
	arco)

; The five columns should contain:
; StringValue, FloatValue, StringValue, FloatValue, FloatValue

(test-assert "expect 0 col of StringValue" (equal?
	'StringValue (cog-type (list-ref arco 0))))

(test-assert "expect 1 col of FloatValue" (equal?
	'FloatValue (cog-type (cog-value-ref (list-ref arco 1) 1))))

(test-assert "expect 2 col of StringValue" (equal?
	'StringValue (cog-type (list-ref arco 2))))

(test-assert "expect 3 col of FloatValue" (equal?
	'FloatValue (cog-type (cog-value-ref (list-ref arco 3) 3))))

(test-assert "expect 4 col of FloatValue" (equal?
	'FloatValue (cog-type (cog-value-ref (list-ref arco 4) 4))))

; -------
; Count, trim and label.
(define trim-count
	(Query (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
		(Present edge-pattern)
		(SexprColumn (Variable "$left-word"))
		(ElementOf (Number 0)
			(IncrementValue (Variable "$left-word")
				(Predicate "counter") (NumberNode 1 0 -0.3)))
		(SexprColumn (Variable "$right-word"))
		(ElementOf (Number 1)
			(IncrementValue (Variable "$right-word")
				(Predicate "counter") (NumberNode 0 1 -0.3)))
		(ElementOf (Number 2)
			(IncrementValue edge-pattern
				(Predicate "counter") (NumberNode 0 0 1)))
		;;; (SexprColumn edge-pattern)
	))

(cog-execute! trim-count)

(format #t "trim-count gives ~A\n"
	(cog-execute! (ValueOf trim-count trim-count)))

(define skinny (cog-execute!
	(TransposeColumn (ValueOf trim-count trim-count))))

(format #t "skinny is ~A\n" skinny)

(define skinhead (cog-value->list skinny))
(test-assert "expect five columns" (equal? 5 (length skinhead)))

(for-each
	(lambda (col)
		(test-assert "expect len=12"
			(equal? 12 (length (cog-value->list col)))))
	skinhead)

; The five columns should contain:
; StringValue, FloatValue, StringValue, FloatValue, FloatValue

(test-assert "expect 0 col of StringValue" (equal?
	'StringValue (cog-type (list-ref skinhead 0))))

(test-assert "expect 1 col of FloatValue" (equal?
	'FloatValue (cog-type (list-ref skinhead 1))))

(test-assert "expect 2 col of StringValue" (equal?
	'StringValue (cog-type (list-ref skinhead 2))))

(test-assert "expect 3 col of FloatValue" (equal?
	'FloatValue (cog-type (list-ref skinhead 3))))

(test-assert "expect 4 col of FloatValue" (equal?
	'FloatValue (cog-type (list-ref skinhead 4))))

; -------
;(define getback (cog-execute!
;	(TransposeColumn (TransposeColumn (ValueOf trim-count trim-count)))))
;
;(format #t "got back ~A\n" getback)

(cog-set-value! trim-count (Predicate "transpose") skinny)
(define getback2 (cog-execute!
	(TransposeColumn (ValueOf trim-count (Predicate "transpose")))))

(format #t "got back2 ~A\n" getback2)

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
