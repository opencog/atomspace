;
; group-by.scm -- Demo of the GroupValue
;
; This example demonstrates the use of the GroupValue to group together
; search results into groupings that are similar to one-another in some
; way. It is inspired by the SQL `GROUP BY` statement, and works in a
; similar fashion.
;
; Background: Search results are always presented as sets of trees.
; These trees are the result of grounding the set of search clauses.
; If search has two or more variables in it, it can be interesting to
; obtain groupings where all results in a group have exactly the same
; grounding for that variable.
;
; The GroupValue is a streaming post-processor that groups query results
; as they arrive. It takes a Lambda predicate that compares pairs of
; results: if the Lambda evaluates to true, the results are placed in
; the same group (bucket). The Lambda receives two result values and
; should compare some component of them for equality.
;
; This demo will use a simple grouping by a shared variable value.

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
; A basic Meet pattern. As results are matched, they are stuffed
; into the GroupValue. The GroupValue receives a LinkValue wrapping
; (Variable "$X") (Variable "$Y") because this is how MeetLinks work.
;
; The GroupValue Lambda compares results pairwise. It uses ElementOf
; to extract the second element (index 1) from each result, and checks
; if they are equal. Results with equal second elements (the category)
; end up in the same bucket.

(define meet-edges
	(Meet
		(VariableList (Variable "$X") (Variable "$Y"))
		(Present
			(Edge (Predicate "property")
				(List (Variable "$X") (Variable "$Y"))))))

; The Lambda compares the $Y component (index 1) of result LinkValues.
; If two results have the same value at index 1, they go in the same group.
(define group-by-category
	(GroupValue
		(Lambda
			(VariableList (Variable "$A") (Variable "$B"))
			(Equal
				(ElementOf (Number 1) (Variable "$A"))
				(ElementOf (Number 1) (Variable "$B"))))))

; Attach the GroupValue to the meet pattern.
; The pattern itself is used as the key for the value.
(cog-set-value! meet-edges meet-edges group-by-category)

; Execute the meet and get grouped results
(define meet-results (cog-execute! meet-edges))

; Print a report.
(format #t "There are ~A groups.\n" (length (cog-value->list meet-results)))
(format #t "The grouped results are:\n~A\n" meet-results)

; Expected output shows 3 groups:
; - colors group: (green colors), (brown colors), (black colors)
; - shapes group: (round shapes), (square shapes), (trident shapes)
; - cloudy group: (vague cloudy)

; -------------------------------------------------------------
; Part the second: A Query pattern with a rewrite rule.
;
; The result of the query is an Edge atom created by the rewrite.
; The GroupValue Lambda extracts the $Y component from the rewritten
; structure using Filter to match the pattern.

(define query-edges
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(Present
			(Edge (Predicate "property")
				(List (Variable "$X") (Variable "$Y"))))
		; Rewrite rule: create a new structure
		(Edge (Predicate "go together")
			(List (Variable "$Y") (Variable "$X")))))

; Extract $Y from (Edge (Predicate "go together") (List $Y $X))
; by using Filter with a pattern that has $y in the first position
(define group-by-rewritten
	(GroupValue
		(Lambda
			(VariableList (Variable "$A") (Variable "$B"))
			(Equal
				(Filter
					(Lambda (Variable "$y")
						(Edge (Predicate "go together")
							(List (Variable "$y") (Type 'Item))))
					(Variable "$A"))
				(Filter
					(Lambda (Variable "$y")
						(Edge (Predicate "go together")
							(List (Variable "$y") (Type 'Item))))
					(Variable "$B"))))))

(cog-set-value! query-edges query-edges group-by-rewritten)

(define query-results (cog-execute! query-edges))

(format #t "\nQuery with rewrite:\n")
(format #t "There are ~A groups.\n" (length (cog-value->list query-results)))
(format #t "The grouped results are:\n~A\n" query-results)

; Expected output shows 3 groups of rewritten edges, grouped by category.

; ------------ That's All, Folks! The End. ------------------
