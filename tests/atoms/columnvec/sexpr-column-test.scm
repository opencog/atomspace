;
; sexpr-column-test.scm -- Verify that SexprColumn works
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "sexpr-column-test")
(test-begin tname)

; --------------------------------------------------------
; Basic test - convert list of atoms to list of strings
(define scol (SexprColumn (List (Concept "foo") (Concept "bar"))))
(define svec (cog-execute! scol))
(format #t "foobar is ~A\n" svec)

(test-assert "foobar-list" (equal? svec
	(StringValue "(Concept \"foo\")" "(Concept \"bar\")")))

; --------------------------------------------------------
; Serialize a single node. Trival case but needs testing.
(define scolnode (SexprColumn (Concept "foo")))
(define snode (cog-execute! scolnode))
(format #t "Single node is ~A\n" snode)
(test-assert "foo-node" (equal? snode
	(StringValue "(Concept \"foo\")")))

; --------------------------------------------------------
; A more typical case: A list of atoms resulting from processing.

(define lv (LinkValue (Concept "foo") (Concept "bar") (Item "zork")))
(cog-set-value! (Anchor "heavy") (Predicate "place") lv)
(define slv
	(SexprColumn (ValueOf (Anchor "heavy") (Predicate "place"))))
(define slvec (cog-execute! slv))
(format #t "LinkValue vec ~A\n" slvec)
(test-assert "foobar-lv" (equal? slvec
	(StringValue
		"(Concept \"foo\")"
		"(Concept \"bar\")"
		"(Item \"zork\")")))

; --------------------------------------------------------
; Complicated case
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

(define mtx
	(Meet (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
		(Present
			(Edge (Predicate "word-pair")
				(List (Variable "$left-word") (Variable "$right-word"))))))

(cog-execute! mtx)
(define mtxgnd (cog-execute! (ValueOf mtx mtx)))
(format #t "Matrix ground ~A\n" mtxgnd)

(define smtx (SexprColumn (ValueOf mtx mtx)))
(define mtxvec (cog-execute! smtx))
(format #t "Matrix vec ~A\n" mtxvec)
(test-assert "matrix size" (equal? 12 (length (cog-value->list mtxvec))))

; --------------------------------------------------------
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
(define smtxpr (SexprColumn (ValueOf mtxpr mtxpr)))
(define prvec (cog-execute! smtxpr))
(format #t "Pair vec ~A\n" prvec)
(test-assert "Pair size" (equal? 12 (length (cog-value->list prvec))))

; --------------------------------------------------------
(test-end tname)
(opencog-test-end)
