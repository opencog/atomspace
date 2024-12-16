;
; group-by.scm -- Demo of the GroupLink
;
; This example demonstrates the use of the GroupLink to group together
; search results into groupings that are similar to one-another in some
; way. It is inspired by the SQL `GROUP BY` statement, and works in a
; similar fashion.
;
; Background: Search results are always presented as sets of trees.
; These trees are the result of grounding the set of search clauses.
; If search has two or more variable in it, it can be interesting to
; obtain groupings where all results in a group have exactly the same
; grounding for that variable.
;
; Such a grouping can be thought of as a connected (hyper-)graph. All
; trees in the group will share the Atom that grounds the grouped
; variable. They are connected through that Atom. Different groupings
; will be disjoint from one another, as they don't share that Atom.
; They may, of course, be connected in other ways, just not through
; the grouping Atom/variable.
;
; Another way of thinking about groupings is that the grouping Atom
; forms a "kernel". Many similar trees are all attached to this kernel,
; these trees differ in various ways from one-another, but they have
; this kernel in common. In this respect, they are all similar.
;
; This demo will use a single, simple grouping variable. Multiple
; variables and complex terms can be used to define a grouping kernel;
; this demo shows only the simplest case.

(use-modules (opencog) (opencog exec))

; Base data to search over

(Edge (Predicate "property") (List (Item "green") (Item "colors")))
(Edge (Predicate "property") (List (Item "brown") (Item "colors")))
(Edge (Predicate "property") (List (Item "black") (Item "colors")))

(Edge (Predicate "property") (List (Item "round") (Item "shapes")))
(Edge (Predicate "property") (List (Item "square") (Item "shapes")))
(Edge (Predicate "property") (List (Item "trident") (Item "shapes")))

(Edge (Predicate "property") (List (Item "vague") (Item "cloudy")))


(define grp-query
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Group (Variable "$Y"))
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y")))))
		(Edge (Predicate "go together")
			(List (Variable "$Y") (Variable "$X")))))

(define query-results (cog-execute! grp-query))
(format #t "There are ~A results\n" (length (cog-value->list query-results)))
(format #t "The query results are ~A\n" query-results)
