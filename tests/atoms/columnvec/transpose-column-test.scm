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

; -------
; Count occurances.
(define mtxpr
	(Query (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
		(Present
			(Edge (Predicate "word-pair")
				(List (Variable "$left-word") (Variable "$right-word"))))
		(IncrementValue (Variable "$left-word")
			(Predicate "counter") (NumberNode 1 0))
		(IncrementValue (Variable "$right-word")
			(Predicate "counter") (NumberNode 0 1))
		(IncrementValue
			(Edge (Predicate "word-pair")
				(List (Variable "$left-word") (Variable "$right-word")))
			(Predicate "counter") (NumberNode 0 0 1))))

(cog-execute! mtxpr)

; (test-assert "Paul" (equal? (FloatValue 1 0)

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
