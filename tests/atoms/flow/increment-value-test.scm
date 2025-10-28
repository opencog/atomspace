;
; increment-value-test.scm -- Verify that IncrementValue works.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "increment-value-test")
(test-begin tname)

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

(test-assert "Paul" (equal? (FloatValue 1 0)
	(cog-value (Item "Paul") (Predicate "counter"))))

(test-assert "bit" (equal? (FloatValue 1 1)
	(cog-value (Item "bit") (Predicate "counter"))))

(test-assert "the" (equal? (FloatValue 2 2)
	(cog-value (Item "the") (Predicate "counter"))))

(test-assert "period" (equal? (FloatValue 0 1)
	(cog-value (Item ".") (Predicate "counter"))))

(test-assert "word-pair" (equal? (FloatValue 0 0 1)
	(cog-value
		(Edge (Predicate "word-pair") (List (Item "leg") (Item "and")))
		(Predicate "counter"))))

; ============================================================
; Test IncrementValueOn - returns Atom instead of Value
; ============================================================

; Define a second set of data for testing IncrementValueOn
(Edge (Predicate "test-pair") (List (Item "Alice") (Item "loves")))
(Edge (Predicate "test-pair") (List (Item "loves") (Item "Bob")))
(Edge (Predicate "test-pair") (List (Item "Bob") (Item "loves")))
(Edge (Predicate "test-pair") (List (Item "loves") (Item "Alice")))

; Query using IncrementValueOn - should return Atoms not Values
(define incr-on-query
	(Query (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))
		(Present
			(Edge (Predicate "test-pair")
				(List (Variable "$left-word") (Variable "$right-word"))))
		(IncrementValueOn (Variable "$left-word")
			(Predicate "on-counter") (NumberNode 1 0))
		(IncrementValueOn (Variable "$right-word")
			(Predicate "on-counter") (NumberNode 0 1))
		(IncrementValueOn
			(Edge (Predicate "test-pair")
				(List (Variable "$left-word") (Variable "$right-word")))
			(Predicate "on-counter") (NumberNode 0 0 1))))

; Execute the query - returns UnisetValue containing Atoms
(define incr-on-result (cog-execute! incr-on-query))

; Verify result is a UnisetValue (Query returns values)
(test-assert "incr-on-result-is-uniset" (equal? 'UnisetValue (cog-type incr-on-result)))

; Verify the values were still incremented correctly
; Alice appears once on left (adds 1,0) and once on right (adds 0,1) = (1,1)
(test-assert "Alice-on" (equal? (FloatValue 1 1)
	(cog-value (Item "Alice") (Predicate "on-counter"))))

(test-assert "Bob-on" (equal? (FloatValue 1 1)
	(cog-value (Item "Bob") (Predicate "on-counter"))))

(test-assert "loves-on" (equal? (FloatValue 2 2)
	(cog-value (Item "loves") (Predicate "on-counter"))))

(test-assert "test-pair-on" (equal? (FloatValue 0 0 1)
	(cog-value
		(Edge (Predicate "test-pair") (List (Item "loves") (Item "Bob")))
		(Predicate "on-counter"))))

; Test standalone IncrementValueOn execution
(define test-atom (Item "test-standalone"))
(define incr-result (cog-execute!
	(IncrementValueOn test-atom (Predicate "standalone-counter") (Number 5))))

; Should return the Atom itself
(test-assert "incr-on-returns-atom" (equal? test-atom incr-result))

; Value should still be incremented
(test-assert "incr-on-value-set" (equal? (FloatValue 5)
	(cog-value test-atom (Predicate "standalone-counter"))))

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
