;
; vector-column.scm -- Pack floating point data into vectors.
;
; The AtomSpace is designed to store arbitrary hypegraphs of any
; shape. When storing data from natural sources, such as biological
; (genomic, proteomic) data, natural language data, and web data,
; such networks are commonly scale-free, and the corresponding
; adjacency matrix is extremely sparse. (That is, the number of
; non-zero elements in the rows (or columns) of the adjacency matrix
; follow a Zipfian distribution. That is, 99.999% of them are zero.)
;
; However, modern theory and practice are oriented around non-sparse
; vectors that can be delivered to a GPU for rapid processing. Thus,
; a reasonable task is to perform a slice through the network graph,
; and deliver that slice to a GPU for further processing. That slice
; can be created using the QueryLink, illustrated in other examples.
; This demo shows how the results of the query can be turned into
; columns, suitable for numeric processing.
;
; Any query (QueryLink, MeetLink, etc.) can be thought of as defining
; an N-tensor, when it has N variables in the query. Thus, a query with
; one variable produces a vector of results. A query with two variables
; produces ... a single list of results, but the distinct values for
; each variable can be thought of as defining the indexes of a matrix.
; For three variables, a 3-tensor is formed.
;
; Tha's all well and fine, but query results typically stay in the
; AtomSpace. To be useful in compute applications, they need to be
; packaged up as vectors. This demo illustrates how this can be done.
;
(use-modules (opencog) (opencog exec))

; ------------------------------------------------------------
; Begin with a trivial case: serialize a list of numbers.

; Here's the list.
(define numli (List
	(NumberNode 1)
	(NumberNode 2)
	(NumberNode 3)
	(NumberNode 4)))

; The FloatColumn can be used to convert this list into a vector
(FloatColumn numli)

; It doesn't become a column until executed:
(define numvec (cog-execute! (FloatColumn numli)))

; Print it out:
(format #t "A vector of floating point numbers: ~A\n" numvec)

; Well, this is pretty boring, except for the fact that, under the
; covers, the FloatValue class is implemented in C++, using
; std::vector<double> to hold this numbers. The std::vector<T>::data()
; method provide a pointer to a raw list of doubles, which can be
; directly handed over to a compute platform for processing.

; ------------------------------------------------------------
; Most AtomSpace processing works with data held int FloatValue's
; and not NumberNodes.  This is as it should be: NumberNodes, since
; they are Nodes, are held directly in the AtomSpace, chewing up RAM
; in the process. Worse: almost all numbers are boring; it is utterly
; pointless to store them as Nodes in the AtomSpace.
;
; To avoid the overhead, the FloatValue is provided. Its small and fast.
; But, like all Values, it is also garbage-collected: once all references
; to it are gone, it disappears. Thus Values must *always* be attached
; to Atoms, if they are to stick around.  This means that working with
; them is a bit trickier: they have to be accessed indirectly, as a
; key->value lookup. The next part reviews how this is done.

; A LinkValue of floats.
(define floli (LinkValue
	(FloatValue 5)
	(FloatValue 6)
	(FloatValue 7)
	(FloatValue 8)))

; Attach that value to an Atom (an AnchorNode, here) using a key
; (a PredicateNode, here).
(cog-set-value! (Anchor "heavy") (Predicate "weight") floli)

; Define some Atomese, that, when executed, will retreive the LinkValue
; (using ValueOfLink) from the given well-known location (the anchor)
; and using the well-known key (the Predicate). After retreival, the
; FloatColumn flattens it all out into a single vector.
(define flocol
	(FloatColumn (ValueOf (Anchor "heavy") (Predicate "weight"))))

; Execute the above, and print the result.
(define flovec (cog-execute! flocol))
(format #t "The float vector is: ~A\n" flovec)

; ------------------------------------------------------------
; Now we dive directly into the deep end.
; This will get complicated, quick.

; Create some "real world" data. For this example, the data is
; extremely uniform: it could just be a plain-old SQL table or
; something. That misses the point, though: the AtomSpace is
; designed to hold extremely irregular data that cannot be captured in
; tables.  But, to aid understanding ... here's a table.
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
; To make the demo realistic, we'll tag each item with some floating
; point data, a "weight" of some sort. Well generate this randomly.
;
; Here's our random-number generator. Each tie it's accessed, it spits
; out a new random number. Just one at a time, that's what the '1' is.
(cog-set-value!
	(Anchor "heavy") (Predicate "randgen") (RandomStream 1))

; We need to get a list of all Items in the AtomSpace. A MeetLink
; will do, for that purpose.
(define item-query
	(Meet
		(TypedVariable (Variable "$word") (Type 'ItemNode))
		(Present (Variable "$word"))))

; Run it.
(cog-execute! item-query)

; The result is cached under the query itself, using the query as the
; key. You can get it manually:
(cog-value item-query item-query)

; Or you can get it automatically:
(cog-execute! (ValueOf item-query item-query))

; Automatically is preferred.
; -------
; Now comes the first hard part. Well draw on the random number
; generator, and tag each item with a random number. (The random number
; generator is a stand-in for a "real compute process" generating
; "real weights".)

; The filter applies the rule to each item returned by the ValueOf
; The rule only accepts ItemNodes, and discard anything else.
; The StreamValueOf samples a single float from the random generator.
; The SetValue attaches that number to each item.
;
; Yes, you cn think of this as a complicated for-loop. That misses the
; point though: streams can be endless, they can be multi-threaded, so
; that the producer is in a different thread than the consumer, and so
; on. These are (in general) multi-threaded producer-consumer streams,
; which is why this seems complicated.
(define tag-items-randomly
	(Filter
		(Rule
			(TypedVariable (Variable "$item") (Type 'ItemNode))
			(Variable "$item")
			(SetValue (Variable "$item") (Predicate "i-weight")
				(StreamValueOf (Anchor "heavy") (Predicate "randgen"))))
		(ValueOf item-query item-query)))

; Anyway, lets run it.
(cog-execute! tag-items-randomly)

; Verify, by hand, that items got tagged.
; List all keys:
(cog-keys (Item "leg"))

; Get the value at that key:
(cog-value (Item "leg") (Predicate "i-weight"))

(format #t "The item ~A has a weight of ~A\n"
	(Item "leg")
	(cog-execute! (ValueOf (Item "leg") (Predicate "i-weight"))))

; Looks random to me.

; ------------------------------------------------------------
; ------------------------------------------------------------
; ------------------------------------------------------------
; Now do the same as above, but for the edges, instead of the
; vertexes. This uses exactly the same techniques; only the query
; is more complex.

(define matrix-of-pairs
	(Query (VariableList
		(TypedVariable (Variable "$left-word") (Type 'ItemNode))
		(TypedVariable (Variable "$right-word") (Type 'ItemNode)))

		; `Present` means it must be (Not (Absent ...)) in the AtomSpace
		(Present
			(Edge (Predicate "word-pair")
				(List (Variable "$left-word") (Variable "$right-word"))))

		; Repeat results. This is a r-write rule that rewrites to iself.
		(Edge (Predicate "word-pair")
			(List (Variable "$left-word") (Variable "$right-word")))))

(cog-execute! matrix-of-pairs)

; As before, you can verify that the result is cached.
(cog-execute! (ValueOf matrix-of-pairs matrix-of-pairs))

; -------
; Paste a bunch of random numbers onto the edges.
(define tag-pairs-randomly
	(Filter
		(Rule
			(Variable "$edge") ; We're not gonna bother with a vardecl.
			(Variable "$edge")
			(SetValue (Variable "$edge") (Predicate "weight")
				(StreamValueOf (Anchor "heavy") (Predicate "randgen"))))
		(ValueOf matrix-of-pairs matrix-of-pairs)))

(cog-execute! tag-pairs-randomly)
; ------------------------------------------------------------
; ------------------------------------------------------------
; ------------------------------------------------------------
; OK, so we've got a bunch of Atoms with a bunch of data attached to
; them. Now, pull that data out, and stick it into vectors.

(define edge-weights
	(FloatColumn
		(Filter
			(Rule
				(Variable "$edge")
				(Variable "$edge")
				(FloatValueOf (Variable "$edge") (Predicate "weight")))
			(ValueOf matrix-of-pairs matrix-of-pairs))))

(define edge-vec (cog-execute! edge-weights))
(format #t "Vector of edge weights: ~A\n" edge-vec)

; Pause for a moment to think about what was acheived here. Some random
; graph with a bunch of randomg data was queried, and a vector of floats
; was created out of it, ready for processing. No small acheivement.

; ------------------------------------------------------------
; Stick a vector of "statistical values" onto the raw data.
; The square and cube of the weights, for this example.
; Then rip these out one column at a time,

(define edge-weight
	(FloatValueOf (Variable "$edge") (Predicate "weight")))

(define tag-pairs-w-stats
	(Filter
		(Rule
			(Variable "$edge")
			(Variable "$edge")
			(SetValue (Variable "$edge") (Predicate "stats")
				(FloatColumn
					edge-weight
					(Times edge-weight edge-weight)
					(Times edge-weight edge-weight edge-weight))))
		(ValueOf mtxpr mtxpr)))

(cog-execute! tag-pairs-w-stats)

; -------
; Go grab the third number from the stats vec, and convert it to a column

(define (grab-col COLNO)
	(FloatColumn
		(Filter
			(Rule
				(Variable "$edge")
				(Variable "$edge")
				(ElementOf (Number COLNO)
					(FloatValueOf (Variable "$edge") (Predicate "stats"))))
		(ValueOf mtxpr mtxpr))))

(define cubecol (grab-col 2))
(define cubevec (cog-execute! cubecol))
(format #t "Cube vect: ~A\n" cubevec)

; Twelve data items, so twelve numbers
(test-assert "cube list length" (equal? 12
	(length (cog-value->list cubevec))))

(define squarecol (grab-col 1))
(define squarevec (cog-execute! squarecol))
(format #t "Square vect: ~A\n" squarevec)

; Twelve data items, so twelve numbers
(test-assert "square list length" (equal? 12
	(length (cog-value->list squarevec))))

(define origcol (grab-col 0))
(define origvec (cog-execute! origcol))
(format #t "Orig vect: ~A\n" origvec)

; Twelve data items, so twelve numbers
(test-assert "orig list length" (equal? 12
	(length (cog-value->list origvec))))

; The first col should be equal to the original weight data.
(test-assert "orig and data equal" (equal? datavec origvec))

; ------------------------------------------------------------
; Super-mega-all-in-one

(define four-col
	(LinkColumn
		(SexprColumn (ValueOf mtxpr mtxpr))
		(grab-col 0) (grab-col 1) (grab-col 2)))

(define four-vec (cog-execute! four-col))
(format #t "Four vect: ~A\n" four-vec)

(define four-list (cog-value->list four-vec))
(test-assert "Four Columns" (equal? 4 (length four-list)))

; First item should be the s-expressions
(test-assert "s-expressions" (equal? (list-ref four-list 0)
	(cog-execute! (SexprColumn (ValueOf mtxpr mtxpr)))))

; The next three should be the earlier number columns
(test-assert "orig col" (equal? (list-ref four-list 1) datavec))
(test-assert "square col" (equal? (list-ref four-list 2) squarevec))
(test-assert "cube col" (equal? (list-ref four-list 3) cubevec))

; ------------------------------------------------------------
