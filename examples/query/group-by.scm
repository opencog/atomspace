;
; group-by.scm -- Demo of the GroupValue
;
; This example demonstrates the use of the GroupValue to group together
; search results into groupings that are similar to one-another in some
; way. It demos the Atomese analog of the SQL `GROUP BY` statement.
;
; The conceptual form of this demo is simple and blunt: perform a
; query, as always, and then post-process the query results, grouping
; them according to a user-defined equivalence relation.
;
; Perhaps the most important saspect of this demo is that it replaces
; an ealier, less flexible, less powerful but "more integrated" design:
; the original implementation used a GroupLink, integrated into the
; query engine.  It increased the over complexity of the query engine,
; while also failing to be as flexible and powerful. And that's why
; this blunt solution of post-processing is offered up: its technically
; better. There is a downside though: the Atomese is more verbose, as
; now the match patterns are specified multiple times: once in the query,
; and again in the post-processing stage.
;
; The GroupValue is a thread-safe container that categorizes elements
; into equivalence classes ("buckets" or "groups"), according to the
; given equivalence relation. As elements are inserted into the
; container, the equivalence relation runs, inserting each into a
; bucket.  The full collection of buckets becomes available when the
; container is closed. Alternately, the buckets streamed out, using
; the FlatStream (not demoed here); but, of course, such bucket may be
; half-full if the consumer removes them faster than the producer
; generates elements. In such cases, there simply won't be any
; opportunity to assign elements to clusters.
;

(use-modules (opencog))

; Create a collection of trees to search over.
; The structure should be obvious, but a few points bear emphasis:
;
; * All of these trees are connected to one-another, as they all
;   have the Atom `(Predicate "property")` in common with one-another.
;   Thus, the below specifies a single connected hypergraph.
;
; * There are two groupings of three that are evident: the colors and
;   the shapes. Ignoring the common `(Predicate "property")`, these
;   groupings are obviously disjoint: none of the colors are shapes,
;   and vice-versa.
;
; The goal of the search query will be to define a search pattern
; that can find the colors and the shapes, and group these results
; together.

(Edge (Predicate "property") (List (Item "green") (Item "colors")))
(Edge (Predicate "property") (List (Item "brown") (Item "colors")))
(Edge (Predicate "property") (List (Item "black") (Item "colors")))

(Edge (Predicate "property") (List (Item "round") (Item "shapes")))
(Edge (Predicate "property") (List (Item "square") (Item "shapes")))
(Edge (Predicate "property") (List (Item "trident") (Item "shapes")))

(Edge (Predicate "property") (List (Item "vague") (Item "cloudy")))

; -------------------------------------------------------------
; A basic Meet pattern. Each match generates a LinkValue holding the
; groundings for the two Variables; this is how MeetLinks work. When a
; MeetLink is executed, each ListValue of groundings is placed into a
; container, where search results are accumulated. By default, this
; container is a UnisetValue: this is a deduplication container that
; ensures no match result is reported more than once. Most of this demo
; is about replacing the default UnisetValue container with the
; GroupValue container,

; The MeetLink
(define meet-edges
	(Meet
		(VariableList (Variable "$X") (Variable "$Y"))
		(Present
			(Edge (Predicate "property")
				(List (Variable "$X") (Variable "$Y"))))))


; The GroupValue container. This container must be provided with an
; equivalence relation that can be used to test equality. This must be
; an evaluatable Atomese function that returns crisp true/false when
; evalatued on a pair of elements.
;
; For this demo, a rather complex LambdaLink is used. It compares
; elements $A and $B, each presumed to be the grounding of $X, $Y
; (above) wrapped up in a LinkValue. It extracts the second element,
; the $Y, using ElementOfLink to access it, and then applies EqualLink
; to test equality.
;
(define group-by-2nd-elt
	(GroupValue
		(Lambda
			(VariableList (Variable "$A") (Variable "$B"))
			(Equal
				(ElementOf (Number 1) (Variable "$A"))
				(ElementOf (Number 1) (Variable "$B"))))))

; Specify that the GroupValue will be used to accumulate query results.
(cog-set-value! meet-edges meet-edges group-by-category)

; Execute the Meet; get grouped results.
(define meet-results (cog-execute! meet-edges))

; Print a report.
(format #t "There are ~A groups.\n" (length (cog-value->list meet-results)))
(format #t "The grouped results are:\n~A\n" (cog-value->list meet-results))

; Expected output shows 3 groups:
; - colors group: (green colors), (brown colors), (black colors)
; - shapes group: (round shapes), (square shapes), (trident shapes)
; - cloudy group: (vague cloudy)

; -------------------------------------------------------------
; Part the second:  A Query pattern with a rewrite rule. This gives an
; opportunity to demonstrate the FilterLink for fishing out data from
; complex structures ("destructuring" the data).
;
(define query-edges
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))

		; The query pattern, same as above.
		(Present
			(Edge (Predicate "property")
				(List (Variable "$X") (Variable "$Y"))))

		; A rewrite to apply, after grounding.
		(Edge (Predicate "go together")
			(List (Variable "$Y") (Variable "$X")))))

; Running the Query will result in a series of "go together" edges being
; generated. We want to compare $Y, just as before. It can be fished out
; by using FliterLink; the extracted results are then presented to the
; EqualLink for comparison.
(define group-edges
	(GroupValue
		(Lambda
			(VariableList (Variable "$A") (Variable "$B"))
			(Equal
				; Filter: Extract $Y from $A which has the shape
				; (Edge (Predicate "go together") (List $Y $X))
				(Filter
					; This Lambda is the filter defintion
					(Lambda (Variable "$y")
						(Edge (Predicate "go together")
							(List (Variable "$y") (Type 'Item))))
					; Ths $A is what the filter is applied to.
					(Variable "$A"))

				; A second filter, for the B-side.
				(Filter
					(Lambda (Variable "$y")
						(Edge (Predicate "go together")
							(List (Variable "$y") (Type 'Item))))
					(Variable "$B"))))))

(cog-set-value! query-edges query-edges group-edges)

(define query-results (cog-execute! query-edges))
; (format #t "The query results are ~A\n" query-results)

(format #t "\nQuery with rewrite:\n")
(format #t "There are ~A groups.\n" (length (cog-value->list query-results)))
(format #t "The grouped results are:\n~A\n" (cog-value->list query-results))

; Expected output shows 3 groups of rewritten edges, grouped by category.

; ------------ That's All, Folks! The End. ------------------
