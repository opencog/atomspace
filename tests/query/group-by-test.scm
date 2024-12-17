;
; group-by-test.scm -- Unit test for the GroupLink
;
; Run this manually by saying "guile -s group-by-test.scm"

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "group-by-test")
(test-begin tname)

; Base data to search over

(Edge (Predicate "property") (List (Item "green") (Item "colors")))
(Edge (Predicate "property") (List (Item "brown") (Item "colors")))
(Edge (Predicate "property") (List (Item "black") (Item "colors")))

(Edge (Predicate "property") (List (Item "round") (Item "shapes")))
(Edge (Predicate "property") (List (Item "square") (Item "shapes")))
(Edge (Predicate "property") (List (Item "trident") (Item "shapes")))

(Edge (Predicate "property") (List (Item "vague") (Item "cloudy")))

; -------------------------------------------------------------
(define grp-meet
	(Meet
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Group (Variable "$Y"))
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y")))))))

(define meet-results (cog-execute! grp-meet))
; (format #t "The meet results are ~A\n" meet-results)
; (format #t "There are ~A results\n" (length (cog-value->list meet-results)))

(test-assert "meet group size"
	(equal? 3 (length (cog-value->list meet-results))))

; -------------------------------------------------------------
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
; (format #t "The query results are ~A\n" query-results)

(test-assert "query group size"
	(equal? 3 (length (cog-value->list query-results))))

; -------------------------------------------------------------

(define grp-range
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Group
				(Variable "$Y")
				(Interval (Number 2) (Number 4)))
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y")))))
		(Variable "$X")))

(define range-results (cog-execute! grp-range))
; (format #t "The range results are ~A\n" range-results)
(test-assert "range group size"
	(equal? 2 (length (cog-value->list range-results))))

; -------------------------------------------------------------

(define collapse-range
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Group
				(Variable "$Y")
				(Interval (Number 2) (Number 4)))
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y")))))
		(Variable "$Y")))

(define collapse-results (cog-execute! collapse-range))
; (format #t "The collapse results are ~A\n" collapse-results)
(test-assert "range collapse size"
	(equal? 2 (length (cog-value->list collapse-results))))

; -------------------------------------------------------------

(test-end tname)
(opencog-test-end)
