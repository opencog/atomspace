;
; flat-stream-test.scm -- Test FlatStream flattening behavior
;
; FlatStream takes a stream source that returns LinkValues and flattens
; them by returning one item at a time. Each access to the stream via
; printing or comparison advances the stream state.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "flat-stream-basic-test")
(test-begin tname)

; Create a nested LinkValue structure with two inner LinkValues
(define syn
   (LinkValue
      (LinkValue
         (FloatValue 1 2 3)
         (FloatValue 4 5 6))

      (LinkValue
         (FloatValue 7 8 9)
         (FloatValue 10 11 12))))

; Attach it to an atom
(cog-set-value! (Anchor "foo") (Predicate "bar") syn)

; Create a FlatStream that will flatten the values
(define fs (FlatStream (ValueOf (Anchor "foo") (Predicate "bar"))))

; Expected values for comparison
(define first-item (LinkValue (FloatValue 1 2 3) (FloatValue 4 5 6)))
(define second-item (LinkValue (FloatValue 7 8 9) (FloatValue 10 11 12)))

; First access - should return first item
(define first-ref (cog-value-ref fs 0))
(format #t "Stream first access: ~A\n" first-ref)
(test-assert "first-access-match" (equal? first-ref first-item))

; Second access - should return second item (not equal to first)
(define second-ref (cog-value-ref fs 0))
(format #t "Stream second access: ~A\n" second-ref)
(test-assert "second-access-match" (equal? second-ref second-item))

; Third access - should be empty.
(format #t "Stream third access: ~A\n" fs)
(test-assert "third-access-match" (equal? fs (LinkValue)))

(test-end tname)

; ---------------------------------------------------------------
; Test with OrderedLink containing Items
; As demonstrated in examples/atomspace/flow-flat.scm
;
(define tname-ordered "flat-stream-ordered-test")
(test-begin tname-ordered)

; Create a list of items similar to flow-flat.scm example
(define item-list
	(OrderedLink
		(Item "a")
		(Item "b")
		(Item "c")
		(Edge
			(Predicate "relation")
			(List (Item "d") (Item "e") (Item "f") (Item "g")))
		(Item "p")
		(Predicate "q")
		(TagNode "z")))

; Wrap the list with the serializer
(define fs-ordered (FlatStream item-list))

; Test sequential access - each access should return the next item wrapped in LinkValue
; Note: format advances stream, then equal? compares current value
(define fs-ref (cog-value-ref fs-ordered 0))
(format #t "Stream access (Item a): ~A\n" fs-ref)
(test-assert "ordered-item-a" (equal? fs-ref (Item "a")))

(set! fs-ref (cog-value-ref fs-ordered 0))
(format #t "Stream access (Item b): ~A\n" fs-ref)
(test-assert "ordered-item-b" (equal? fs-ref (Item "b")))

(set! fs-ref (cog-value-ref fs-ordered 0))
(format #t "Stream access (Item c): ~A\n" fs-ref)
(test-assert "ordered-item-c" (equal? fs-ref (Item "c")))

; Next item is an Edge
(define expected-edge
	(Edge
		(Predicate "relation")
		(List (Item "d") (Item "e") (Item "f") (Item "g"))))

(set! fs-ref (cog-value-ref fs-ordered 0))
(format #t "Stream access (Edge): ~A\n" fs-ref)
(test-assert "ordered-edge" (equal? fs-ref expected-edge))

; Continue with remaining items
(set! fs-ref (cog-value-ref fs-ordered 0))
(format #t "Stream access (Item p): ~A\n" fs-ref)
(test-assert "ordered-item-p" (equal? fs-ref (Item "p")))

(set! fs-ref (cog-value-ref fs-ordered 0))
(format #t "Stream access (Predicate q): ~A\n" fs-ref)
(test-assert "ordered-predicate-q" (equal? fs-ref (Predicate "q")))

(set! fs-ref (cog-value-ref fs-ordered 0))
(format #t "Stream access (TagNode z): ~A\n" fs-ref)
(test-assert "ordered-tagnode-z" (equal? fs-ref (TagNode "z")))

; After reaching the end, it should wrap around to the first item
(format #t "Expect end-of-stream: ~A\n" fs-ordered)
(test-assert "ordered-eof" (equal? fs-ordered (LinkValue)))

(test-end tname-ordered)

; ---------------------------------------------------------------
; Test SetValue with CollectionOf pattern
; As demonstrated in examples/atomspace/flow-flat.scm
;
(define tname-setvalue "flat-stream-setvalue-test")
(test-begin tname-setvalue)

; Create a simple list
(define simple-list
	(OrderedLink
		(Concept "first")
		(Concept "second")
		(Concept "third")))

; Use SetValue with CollectionOf to create stream in "pure Atomese"
; Capture as "junk" to avoid accidental stream deref.
(define junk
	(cog-execute!
		(SetValue (Concept "stream-test") (Predicate "location")
			(LinkSignature (Type 'FlatStream) simple-list))))

; Create ValueOf reference to the stream
(define stream
	(cog-execute! (ValueOf (Concept "stream-test") (Predicate "location"))))

; Test that cog-execute! advances the stream (returns FlatStream that wraps LinkValues)
(define stream-ref (cog-value-ref stream 0))
(format #t "SetValue test first: ~A\n" stream-ref)
(test-assert "setvalue-first"
	(equal? stream-ref (Concept "first")))

(format #t "SetValue test second: ~A\n" stream)
(test-assert "setvalue-second"
	(equal? stream (LinkValue (Concept "second"))))

(set! stream-ref (cog-value-ref stream 0))
(format #t "SetValue test third: ~A\n" stream-ref)
(test-assert "setvalue-third"
	(equal? stream-ref (Concept "third")))

(define stream-end (cog-value->list stream))
(format #t "SetValue test eof: ~A\n" stream-end)
(test-assert "setvalue-end"
	(equal? stream-end '()))

; Should wrap around.  Why? Because FlatStream recurses on us here;
; it treats itself as an infinite stream.
(format #t "SetValue test wrap: ~A\n" stream)
(test-assert "setvalue-wrap-first"
	(equal? stream (LinkValue (Concept "first"))))

(test-end tname-setvalue)

; ---------------------------------------------------------------
; Test multiple stream references and independence
;
(define tname-multi "flat-stream-multi-test")
(test-begin tname-multi)

; Create two independent streams from the same source
(define list-abc
	(OrderedLink
		(Concept "A")
		(Concept "B")
		(Concept "C")))

(define stream-1 (FlatStream list-abc))
(define stream-2 (FlatStream list-abc))

; Advance stream-1 twice
(format #t "Stream-1 access A: ~A\n" stream-1)
(test-assert "multi-s1-a" (equal? stream-1 (LinkValue (Concept "A"))))

(format #t "Stream-1 access B: ~A\n" stream-1)
(test-assert "multi-s1-b" (equal? stream-1 (LinkValue (Concept "B"))))

; stream-2 should still be at the start
(format #t "Stream-2 access A: ~A\n" stream-2)
(test-assert "multi-s2-a" (equal? stream-2 (LinkValue (Concept "A"))))

; Continue advancing stream-1
(format #t "Stream-1 access C: ~A\n" stream-1)
(test-assert "multi-s1-c" (equal? stream-1 (LinkValue (Concept "C"))))

; stream-2 should be at second position
(format #t "Stream-2 access B: ~A\n" stream-2)
(test-assert "multi-s2-b" (equal? stream-2 (LinkValue (Concept "B"))))

(test-end tname-multi)

; ---------------------------------------------------------------
; Test cog-value->list interaction with stream
;
(define tname-valuelist "flat-stream-valuelist-test")
(test-begin tname-valuelist)

(define vlist-stream
	(FlatStream
		(OrderedLink
			(Number 1)
			(Number 2)
			(Number 3))))

; First access - returns LinkValue wrapping Number
(format #t "ValueList test first: ~A\n" vlist-stream)
(test-assert "valuelist-num-1" (equal? vlist-stream (LinkValue (Number 1))))

; cog-value->list also advances the stream and extracts the unwrapped value
(format #t "ValueList test second (via cog-value->list): ~A\n" vlist-stream)
(define vlist-result (cog-value->list vlist-stream))
(test-assert "valuelist-advances"
	(equal? (car vlist-result) (Number 3)))

(test-end tname-valuelist)

(opencog-test-end)
