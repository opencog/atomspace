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
; The goal of this demo is to show how to perform processing on live
; data streams. A "data stream" is meant to be some sensory device: some
; interface to the external environment, delivering measurement signals
; of some kind. It is presumed to have been turned into a stream of
; Values by the time it gets to this demo.  This demo applies a filter
; to that stream, and then performs some processing on the result. The
; last demo, at the bottom of this file, increments a count attached
; to Atoms held in the AtomSpace, based on what arrived in the stream.
; This is an "observation count": how many times was his input observed
; in the input steam?
;
; See https://wiki.opencog.org/w/FilterLink for general documentation.
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

		; The sequence of Values to be filtered by above.
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

		; The sequence of Values to be filtered by above.
		(ValueOf (Node "some place") (Predicate "some key")))
)

; This should return lists of edges in each sentence.
(cog-execute! get-parse-edges)

; -----------
; Same as above, but after extracting the list of edges, place them
; into a different location. This particular demo is kind-of weird,
; and breaks the general distinction between Atoms and Values.
; However, its a worth demo, because it is a stepping stone to the
; demo after this. And so, the explanation:
;
; As above, a collection of Values is yanked out of a specific
; location; they are filtered so that only the desired values are
; returned. As the result was variable-length, a GlobNode was used
; for the pattern matching, and the resulting collection was wrapped
; in a LinkValue (as is entirely appropriate for a collection of
; Values.) As there was more than one of these globs, a list of them
; were returned (so, a list of lists).
;
; In the below, an additional transformation step will be applied:
; a rewrite, to change the general form. For the demo, the rewrite
; will be simple: wrap the results with a label, a ConceptNode,
; and then stick them into a link, and OrderdLink for this demo.
;
; The OrderedLink is meant to be a stand-in for something executable:
; some link that would accept a list of values, and do something with
; them: maybe add or subtract them, or apply some other function. But,
; to show the general idea, an OrderedLink will be used in place of a
; function. This creates a problem, though: Values cannot be placed in
; Links. As a work-around, for function argument passing, a special
; wrapper is created: the ValueShimLink. It can hold a Value, and since
; it is an Atom, it can be placed inside of Links.
;
; Now, for the weird part: it CANNOT be placed into the AtomSpace! It's
; an autogenerated intermediate wrapper, employed in certain special
; cases of pipeline processing of values. It will be printed in the demo
; below; but you will note that none of the created Atoms are actually
; in the AtomSpace.  They remain floating, as Values. The demo after
; this will do something  with them.
;
; One more thing: a brief review of the RuleLink. The RuleLink resembles
; a logical implication. It has the form of a rewrite rule P(X)->Q(X),
; where, if the input matches the form P(X), then the rwrite is applied
; to create Q(X). Here, X is one or more variables that are matched in
; the pattern P(X). The form of a RuleLink is
;    (RuleLink (Variable decls) (body to match) (rewrite to generate))
; See https://wiki.opencog.org/w/RuleLink for details.

(define rewrite-parse-edges
	(Filter
		(Rule
			(Glob "$x")
			(LinkSignature
				(Type 'LinkValue)
				(Type 'LinkValue)
				(LinkSignature
					(Type 'LinkValue)
					(Concept "parse")
					(Glob "$x")))
			(OrderedLink (Concept "bunch of edges") (Glob "$x")))

		; The sequence of Values to be filtered by above.
		(ValueOf (Node "some place") (Predicate "some key")))
)

; This should return lists of edges in each sentence.
(cog-execute! rewrite-parse-edges)

; -----------
; Same as above, but after extracting the list of edges, increment
; the count on them. This proceeds as the demo above, but replaces
; the OrderedLink by another Filter that can process it's arguments
; and perform an increment.
;
; Define an increment function, where the incrementation is done in
; Atomese, not in scheme.
(define (incr-cnt edge)
	(SetValue edge (Predicate "count")    ; Set vector on the Atom "edge"
		(Plus (Number 0 0 1)               ; Add a vector to the count vect.
			(FloatValueOf edge (Predicate "count")
				(FloatValueOf (Number 0 0 0)))))) ; Default for first time.

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
			(extract (Glob "$x")))           ; The complicated rewrite

		; The sequence of Values to be filtered by above.
		(ValueOf (Node "some place") (Predicate "some key")))
)

; This should return lists of updated counts.
(cog-execute! increment-parse-edges)

; Verify that the count really was incremented.
;
; One of the edges:
(define e (Edge (Bond "pair") (List (Concept "this") (Concept "is"))))

; A list of the keys on that edge:
(cog-keys e)

; The value attached to that key:
(cog-value e (Predicate "count"))

; Increment again.
(cog-execute! increment-parse-edges)
; Verify that the count went up.
(cog-value e (Predicate "count"))

; Increment again.
(cog-execute! increment-parse-edges)
; Verify that the count went up.
(cog-value e (Predicate "count"))

; THE END. That's all folks!
