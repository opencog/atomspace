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

; Null-terminate, else the FlatStream below will loop forever.
(define test-data
	(LinkValue
		(Concept "A")
		(Concept "C")
		(Concept "A")
		(Concept "B")
		(Concept "A")
		(VoidValue)))

(define data-anchor (Anchor "process-anchor"))
(cog-set-value! data-anchor (Predicate "test-data") test-data)

; Use FlatStream to dole out the values one by one.
; i.e. to turn the static data above into an actual stream.
(define (reset-stream)
	(cog-execute!
		(SetValue data-anchor (Predicate "data-stream")
			(LinkSignature (Type 'FlatStream)
				(ValueOf data-anchor (Predicate "test-data"))))))

; Call this to rewind back to the start.
(reset-stream)

(define data-stream
	(ValueOf data-anchor (Predicate "data-stream")))

; (define strm (cog-execute! data-stream))
; (format #t "It is ~A\n" strm)

; ------------------------------------------------------------
; Create a FilterLink that counts each atom and increments total

(define counter
	(Filter
		(Rule
			(TypedVariable (Variable "$item") (Type 'ConceptNode))
			(Variable "$item")

			; Odd ... by default, FilterLink wraps these
			; results in a ListLink... let's avoid that.
			(LinkSignature (Type 'LinkValue)
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

; (cog-execute! counter)

; ------------------------------------------------------------
; Wrap the counter in a DrainLink and execute it

; Brute-force drain
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
; Do it again, but indirectly.

(cog-execute!
	(SetValue data-anchor (Predicate "counter-stream")
		(DontExec counter)))

(define counter-stream
	(ValueOf data-anchor (Predicate "counter-stream")))

; Rewind back to the start.
(reset-stream)

; Indirect drain from the anchor point
(define drainer (Drain counter-stream))

; Execute the DrainLink - this should process all 5 items
(cog-execute! drainer)

; ------------------------------------------------------------
; Verify the counts

; A should appear 3 more times
(test-assert "count-A" (equal? (FloatValue 6)
	(cog-value (Concept "A") (Predicate "item-count"))))

; B should appear 1 more time
(test-assert "count-B" (equal? (FloatValue 2)
	(cog-value (Concept "B") (Predicate "item-count"))))

; C should appear 1 more time
(test-assert "count-C" (equal? (FloatValue 2)
	(cog-value (Concept "C") (Predicate "item-count"))))

; Total count on anchor should increment by 5
(test-assert "total-count" (equal? (FloatValue 10)
	(cog-value data-anchor (Predicate "total-count"))))

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
