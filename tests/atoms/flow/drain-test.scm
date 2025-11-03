;
; drain-test.scm -- Verify that DrainLink works.
;
; Test that DrainLink exhausts a stream, processing all values.
; Creates a stream of ConceptNodes (A C A B A) and counts them
; both individually and in total.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "drain-test")
(test-begin tname)

; ------------------------------------------------------------
; Create test data: a LinkValue with five ConceptNodes

(define test-data
	(LinkValue
		(Concept "A")
		(Concept "C")
		(Concept "A")
		(Concept "B")
		(Concept "A")))

; Attach this LinkValue to an Anchor
(define data-anchor (Anchor "test-data-anchor"))
(cog-set-value! data-anchor (Predicate "data-stream") test-data)

; Create a CollectionOf to turn the LinkValue into a stream
(define data-stream
	(CollectionOfLink (Type 'FlatStream)
		(OrderedLink
			(ValueOf data-anchor (Predicate "data-stream")))))

; ------------------------------------------------------------
; Create a FilterLink that counts each atom and increments total

(define counter
	(Filter
		(Rule
			(TypedVariable (Variable "$item") (Type 'ConceptNode))
			(Variable "$item")
			(List
				; Increment count on the individual atom
				(IncrementValueOn
					(Variable "$item")
					(Predicate "item-count")
					(Number 1))
				; Increment grand total on the anchor
				(IncrementValueOn
					data-anchor
					(Predicate "total-count")
					(Number 1))))
		data-stream))

; ------------------------------------------------------------
; Wrap the counter in a DrainLink and execute it

(define drainer (Drain counter))

; Execute the DrainLink - this should process all 5 items
(cog-execute! drainer)

; ------------------------------------------------------------
; Verify the counts

; A should appear 3 times
(test-assert "count-A" (equal? (FloatValue 3)
	(cog-value (Concept "A") (Predicate "item-count"))))

; B should appear 1 time
(test-assert "count-B" (equal? (FloatValue 1)
	(cog-value (Concept "B") (Predicate "item-count"))))

; C should appear 1 time
(test-assert "count-C" (equal? (FloatValue 1)
	(cog-value (Concept "C") (Predicate "item-count"))))

; Total count on anchor should be 5
(test-assert "total-count" (equal? (FloatValue 5)
	(cog-value data-anchor (Predicate "total-count"))))

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
