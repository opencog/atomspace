;
; group-by.scm -- Demo of the GroupLink
;

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
