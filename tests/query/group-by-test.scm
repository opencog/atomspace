;
; group-by-test.scm
;
; Unit test for the GroupBy link

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

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
		(GroupBy (Variable "$Y"))
		(Present 
			(Edge (Predicate "property")
				(List (Variable "$X") (Variable "$Y"))))
		(Edge (Predicate "property")
			(List (Variable "$X") (Variable "$Y")))))
