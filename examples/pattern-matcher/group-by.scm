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
; A third way of thinking of groupings is as "local for-all clauses".
; Thus, for all members in a group, the property specified in the
; grouping kernel holds. In this sense, GroupLink is a "local" version
; of AlwaysLink. The AlwaysLink asks that all search results must have
; in common the specified clause, or, equivalently, that there must be
; one and only one group. The GroupLink relaxes this demand for there
; to be only one, and presents several groupings, as these occur.
;
; This demo will use a single, simple grouping variable. Multiple
; variables and complex terms can be used to define a grouping kernel;
; this demo shows only the simplest case.

(use-modules (opencog) (opencog exec))

; Create a collection of trees to search over.
; The structure should be obvious, but a few points bear empahsis:
;
; * All of these trees are connected to one-another, as they all
;   have the Atom `(Predicate "property")` in common with one-another.
;   Thus, the below specifies a single connected hypergraph.
;
; * There are two groupings of three that are evident: the colors and
;   the shapes. Ignoring the common `(Predicate "property")`, these
;   groupings are obviously disjoint: none of the colors are shapes,
;   and vice-versa. Each grouping forms a connected sub-hypergraph,
;   in that the Atom `(Item "colors")` is shared in common by all
;   of the trees that ... share it.
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

(Edge (Predicate "le grande foobar") (List (Item "blob") (Item "shapes")))

(Edge (Predicate "property") (List (Item "vague") (Item "cloudy")))

; Define a query that will look for relations having "property", and
; group them together by commonality in the second position of the
; property: group them by commonality in `(Variable "$Y")`.

(define grp-query
	(Query
		; Variable declarations (optional)
		(VariableList (Variable "$X") (Variable "$Y"))

		; Use an AndLink to unite all the search clauses
		(And

			; The "property" must be present in the AtomSpace.
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y"))))

			; The search results will be grouped together by having
			; a common value of $Y
			(Group (Variable "$Y")))

		; The QueryLink is a kind of rewrite-rule; the variable
		; groundings can be used to create new structures. For this
		; demo, some nonsense Implication & Evaluation links are
		; created. You don't want to use Implication like this in
		; practice, but visually, it works for this demo.
		(Evaluation (Concept "things that go together")
			(Implication (Variable "$Y") (Variable "$X")))))

; Perform the query, and put the results in a scheme object.
(define query-results (cog-execute! grp-query))

; Print a report.
(format #t "There are ~A results.\n" (length (cog-value->list query-results)))
(format #t "The query results are:\n~A\n" query-results)

; -------------------------------------------------------------
; Part the second.
; This is a variant of the above, perhaps showing more clearly and
; distinctly thr grouping effect.

(define grp-set
	(Bind
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y"))))
			(Group (Variable "$Y")))
		(Variable "$X")))

(define set-results (cog-execute! grp-set))
(format #t "The groupings are:\n~A\n" set-results)

; ------------ That's All, Folks! The End. ------------------
