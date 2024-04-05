;
; filter-value.scm -- Using FilterLink for processing data streams
;
; The FilterLink implements a link type analogous to the `filter-map`
; function commonly found in functional programming languages, such as
; the scheme srfi-1 `filter-map`, or `map` in haskell.
;
; This demo focuses on filtering data streams; please review `filter.scm`
; for a more basic introduction to filtering.
;
(use-modules (opencog) (opencog exec))

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
;
; The result should be a match of (Variable $x) to the LinkValue
; containing the list of words in the sentence.
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
(cog-execute! get-parse-sentences)

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
(cog-execute! get-parse-edges)

; THE END. That's all folks!
