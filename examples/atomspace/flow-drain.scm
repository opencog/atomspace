;
; flow-drain.scm -- Run a stream until it is empty.
;
; The previous flow demos all illustrate how to create a flow,
; and then, for demo purposes, the flow is advanced one step at
; a time. In practice, one will want to set up flow that run
; forever. There are several ways of doing this. One of the most
; convenient ways is to use DrainLink to run a stream dry.
;
; The demo below moks up a short finite stream, by declaring a
; static array, and then doling out from that array, one by one,
; via the FlatStream iterator. The items handed out will each
; have their use count incremented by one, and a total count
; incremented as well. That the stream drained to completion can
; be checked by looking at these counts at the end.
;
(use-modules (opencog) (opencog exec))

; Create a static list of five items.
; Null-terminate the list, as otherwise the FlatStream below
; will loop forever.
(define test-data
	(LinkValue
		(Concept "A")
		(Concept "C")
		(Concept "A")
		(Concept "B")
		(Concept "A")
		(VoidValue)))

; Anchor this at a "well-known location".
(define data-anchor (Anchor "process-anchor"))
(cog-set-value! data-anchor (Predicate "test-data") test-data)

; Mock up a flowing stream by using FlatStream to dole out
; the items, one by one. This converts the static list above
; into an actual stream.
(define stream-maker
	(LinkSignature (Type 'FlatStream)
		(ValueOf data-anchor (Predicate "test-data"))))

; You can test this as follows:
; (define stream-iter (cog-execute! stream-maker))
; stream-iter
; stream-iter
; stream-iter
; Of course if you do this five times, you'll drain the stream
; and you'll have to start again.

; Place the iterator in a well-known location. Make it easy to rewind.
(define (reset-stream)
	(cog-execute!
		(SetValue data-anchor (Predicate "data-stream") stream-maker)))

; Call this to rewind back to the start.
(reset-stream)

; Define a location where the stream is accessed.
(define data-stream
	(ValueOf data-anchor (Predicate "data-stream")))

; Try it, if you want.
; (define strm (cog-execute! data-stream))
; (format #t "It is ~A\n" strm)

; ------------------------------------------------------------
; Create a FilterLink that counts each atom and increments a total.

(define counter
	(Filter
		(Rule
			; Each item in the stream is a ConceptNode, Duhh.
			(TypedVariable (Variable "$item") (Type 'ConceptNode))

			; Match that item.
			(Variable "$item")

			; Rewrite the item.
			; By default, FilterLink wraps these results
			; in a ListLink... Let's avoid that.
			(LinkSignature (Type 'LinkValue)

				; Increment count on the individual atom.
				(IncrementValueOn
					(Variable "$item")
					(Predicate "item-count")
					(Number 1))

				; Increment grand total on the anchor.
				(IncrementValueOn
					data-anchor
					(Predicate "total-count")
					(Number 1))))
		data-stream))

; Try it, you'll like it!
; (cog-execute! counter)

; ------------------------------------------------------------
; Wrap the counter in a DrainLink and execute it

; Brute-force drain.
(define drainer (Drain counter))

; Execute the DrainLink - this should process all 5 items.
; Actually, only four, because the demo setup accidentally
; unqueued (at least) one item, already.
; (cog-execute! drainer)

; The above should finish instantly. For infinite streams,
; you will want to use ParallelLink, which will run the drainer
; in it's own thread, like so:
(cog-execute! (ParallelLink drainer))

; Umm, well give the above thread a chance to run, before
; trying to look at the results!
(sleep 1)

; ------------------------------------------------------------
; Verify the counts

; A should appear 3 times
(format #t "count-A = ~A\n"
	(cog-value (Concept "A") (Predicate "item-count")))

; B should appear 1 time
(format #t "count-B = ~A\n"
	(cog-value (Concept "B") (Predicate "item-count")))

; C should appear 1 time
(format #t "count-C = ~A\n"
	(cog-value (Concept "C") (Predicate "item-count")))

; Total count on anchor should be 5
(format #t "total-count = ~A\n"
	(cog-value data-anchor (Predicate "total-count")))

; ------------------------------------------------------------
; Do it again, but indirectly. That is, anchor the counter
; somewhere, and access it via the anchor.

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
(format #t "count-A = ~A\n"
	(cog-value (Concept "A") (Predicate "item-count")))

; B should appear 1 more time
(format #t "count-B = ~A\n"
	(cog-value (Concept "B") (Predicate "item-count")))

; C should appear 1 more time
(format #t "count-C = ~A\n"
	(cog-value (Concept "C") (Predicate "item-count")))

; Total count on anchor should increment by 5
(format #t "total-count = ~A\n"
	(cog-value data-anchor (Predicate "total-count")))

; ---------------- The End! That's All, Folks! ---------------
; ------------------------------------------------------------
