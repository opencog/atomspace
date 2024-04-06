;
; filter-value-test.scm -- Verify that the demo filter-value.scm works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "filter-value-test")
(test-begin tname)

; Begin by creating a mockup of a complicated data stream. What makes
; this "complicated" is that it will consist of nested LinkValues,
; a tree of LinkValues, with different kinds of data in the tree.

(define word-list-a
	(LinkValue (Concept "this") (Concept "is") (Concept "a") (Concept "test")))
(define word-list-b
	(LinkValue (Concept "moar") (Concept "stuffs") (Concept "ok")))

(define edge-a1 (Edge (Bond "pair") (List (Concept "this") (Concept "is"))))
(define edge-a2 (Edge (Bond "pair") (List (Concept "is") (Concept "test"))))
(define edge-a3 (Edge (Bond "pair") (List (Concept "a") (Concept "test"))))
(define edge-b1 (Edge (Bond "pear") (List (Concept "moar") (Concept "stuffs"))))
(define edge-b2 (Edge (Bond "pear") (List (Concept "moar") (Concept "ok"))))

(define tree-list
	(LinkValue
		(LinkValue
			(LinkValue (Concept "sentence") word-list-a)
			(LinkValue (Concept "parse") edge-a1 edge-a2 edge-a3))
		(LinkValue
			(LinkValue (Concept "sentence") word-list-b)
			(LinkValue (Concept "parse") edge-b1 edge-b2))))

; Place the tree-list in a well-known location, where we can find it.
(cog-set-value!
	(Node "some place") (Predicate "some key") tree-list)

; -----------
; Define a pattern that will extract all of the sentences
; from the tree list. The tree-list is a list of pairs.
; The first elt in the pair has a label called (Concept "sentence")
; and so the filter pattern reaches into the pair to grab that.
(define get-parse-sentences
	(Filter
		(Lambda
			(Variable "$x")
			(LinkSignature
				(Type 'LinkValue)
				(LinkSignature
					(Type 'LinkValue)
					(Concept "sentence")
					(Variable "$x"))
				(Type 'LinkValue)))

		; The sequence of Values to be filterd by above.
		(ValueOf (Node "some place") (Predicate "some key")))
)

; This should return lists of words in each sentence.
(define wrd-list (cog-execute! get-parse-sentences))

(test-assert "pair of sentences"
	(equal? wrd-list (LinkValue word-list-a word-list-b)))

; -----------
; Same as above, but extract the list of edges. This uses a GlobNode
; because the number of edges is variable.
(define get-parse-edges
	(Filter
		(Lambda
			(Glob "$x")
			(LinkSignature
				(Type 'LinkValue)
				(Type 'LinkValue)
				(LinkSignature
					(Type 'LinkValue)
					(Concept "parse")
					(Glob "$x"))))

		; The sequence of Values to be filterd by above.
		(ValueOf (Node "some place") (Predicate "some key")))
)

; This should return lists of edges in each sentence.
(define edge-list (cog-execute! get-parse-edges))
(test-assert "edge list"
	(equal? edge-list
		(LinkValue
			(LinkValue edge-a1 edge-a2 edge-a3)
			(LinkValue edge-b1 edge-b2))))

; -----------
; Same as above, but increment.
(define (incr-cnt edge)
	(SetValue edge (Predicate "count")    ; Set vector on the Atom "edge"
		(Plus (Number 0 0 1)               ; Add a vector to the count vect.
			(Cond (FloatValueOf edge (Predicate "count")) ; Is there a key?
				(FloatValueOf edge (Predicate "count"))    ; If yes, get it.
				(FloatValueOf (Number 0 0 0))))))          ; If no, set to zero.

; Extract edges from the stream, and use above to increment.
(define (extract stuff)
	(Filter
		(Rule                               ; Definition of rewrite rule.
			(Variable "$edge")               ; Variable declaration.
			(Variable "$edge")               ; What to match. (Everything)
			(incr-cnt (Variable "$edge")))   ; What to do with it.
		stuff))                             ; What to apply the rule to.

(define increment-parse-edges
	(Filter
		(Rule                               ; Just as in earlier demo
			(Glob "$x")
			(LinkSignature
				(Type 'LinkValue)
				(Type 'LinkValue)
				(LinkSignature
					(Type 'LinkValue)
					(Concept "parse")
					(Glob "$x")))
			(extract (Glob "$x")))           ; The complicate rewrite

		; The sequence of Values to be filterd by above.
		(ValueOf (Node "some place") (Predicate "some key")))
)

(cog-execute! increment-parse-edges)

; Verify that the count really was incremented.
(define key (Predicate "count"))
(test-assert "cnt-a1" (equal?  (cog-value edge-a1 key) (FloatValue 0 0 1)))
(test-assert "cnt-a2" (equal?  (cog-value edge-a2 key) (FloatValue 0 0 1)))
(test-assert "cnt-a3" (equal?  (cog-value edge-a3 key) (FloatValue 0 0 1)))
(test-assert "cnt-b1" (equal?  (cog-value edge-b1 key) (FloatValue 0 0 1)))
(test-assert "cnt-b2" (equal?  (cog-value edge-b2 key) (FloatValue 0 0 1)))

; Do it again.
(cog-execute! increment-parse-edges)
(test-assert "cnt-a1" (equal?  (cog-value edge-a1 key) (FloatValue 0 0 2)))
(test-assert "cnt-a2" (equal?  (cog-value edge-a2 key) (FloatValue 0 0 2)))
(test-assert "cnt-a3" (equal?  (cog-value edge-a3 key) (FloatValue 0 0 2)))
(test-assert "cnt-b1" (equal?  (cog-value edge-b1 key) (FloatValue 0 0 2)))
(test-assert "cnt-b2" (equal?  (cog-value edge-b2 key) (FloatValue 0 0 2)))

; Do it again.
(cog-execute! increment-parse-edges)
(test-assert "cnt-a1" (equal?  (cog-value edge-a1 key) (FloatValue 0 0 3)))
(test-assert "cnt-a2" (equal?  (cog-value edge-a2 key) (FloatValue 0 0 3)))
(test-assert "cnt-a3" (equal?  (cog-value edge-a3 key) (FloatValue 0 0 3)))
(test-assert "cnt-b1" (equal?  (cog-value edge-b1 key) (FloatValue 0 0 3)))
(test-assert "cnt-b2" (equal?  (cog-value edge-b2 key) (FloatValue 0 0 3)))

(test-end tname)

(opencog-test-end)
