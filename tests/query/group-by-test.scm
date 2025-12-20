#! /usr/bin/env guile
-s
!#
;
; group-by-test.scm -- Unit test for the GroupLink
; Except GroupLink is now obsolete, and is replaced by GroupValue,
; which is used to perform post-query filtering. This complicates
; the tests considerably, and changes their nature into being more
; of a test of GroupValue, than of the query itself. The changes
; aka issues:
; *) The Variables in the GroupValue Lambda have no relationship
;    to the Variables in the pattern.
; *) The grouping can be specified by *any* lambda, and not equality.
;    The GroupValue is far more general.
;
; Run this manually by saying "./group-by-test.scm"

(use-modules (opencog))
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
; A basic Meet pattern. As results are matched, they are stuffed
; into the GroupValue. The GroupValue receives a ListValue wrapping
; (Variable "$X") (Variable "$Y") because this is how MeetLinks work.
; The GroupValue extracts the second elt in the list using ElementOf,
; and then performs an equality check on that.
;
(define meet-edges
	(Meet
		(VariableList (Variable "$X") (Variable "$Y"))
		(Present
			(Edge (Predicate "property")
				(List (Variable "$X") (Variable "$Y"))))))

; The Lambda compares the $Y component (index 1) of result LinkValues
(define group-pairs
	(GroupValue
		(Lambda
			(VariableList (Variable "$A") (Variable "$B"))
			(Equal
				(ElementOf (Number 1) (Variable "$A"))
				(ElementOf (Number 1) (Variable "$B"))))))

(cog-set-value! meet-edges meet-edges group-pairs)

(define meet-results (cog-execute! meet-edges))
; (format #t "The meet results are ~A\n" meet-results)
; (format #t "There are ~A results\n" (length (cog-value->list meet-results)))

(test-assert "meet group size"
	(equal? 3 (length (cog-value->list meet-results))))

; -------------------------------------------------------------
; Same as above, but uses a Filter to extract the second element.
; i.e. the pattern is entirely unchanged, we just rework the GroupValue.

(define destructure-filter
	(GroupValue
		(Lambda
			(VariableList (Variable "$A") (Variable "$B"))
			(Equal
				(Filter
					(Lambda (Variable "$x")
						(LinkSignature (Type 'LinkValue) (Type 'Item) (Variable "$x")))
					(Set (Variable "$A")))
				(Filter
					(Lambda (Variable "$x")
						(LinkSignature (Type 'LinkValue) (Type 'Item) (Variable "$x")))
					(Set (Variable "$B")))))))

(cog-set-value! meet-edges meet-edges destructure-filter)

(define meet-destr (cog-execute! meet-edges))

(test-assert "meet destructure group size"
	(equal? 3 (length (cog-value->list meet-destr))))

; -------------------------------------------------------------
; A Query pattern with a rewrite rule. The result of the query is
; an Edge atom. The GroupValue Lambda extracts the first element
; of the List inside the Edge (which is $Y) using Filter.
;
(define query-edges
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(Present
			(Edge (Predicate "property")
				(List (Variable "$X") (Variable "$Y"))))
		(Edge (Predicate "go together")
			(List (Variable "$Y") (Variable "$X")))))

; Extract $Y from (Edge (Predicate "go together") (List $Y $X))
(define group-edges
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

(cog-set-value! query-edges query-edges group-edges)

(define query-results (cog-execute! query-edges))
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

(define unbounded-range
	(Query
		(VariableList (Variable "$X") (Variable "$Y"))
		(And
			(Group
				(Variable "$Y")
				(Interval (Number 2) (Number -1)))
			(Present
				(Edge (Predicate "property")
					(List (Variable "$X") (Variable "$Y")))))
		(Variable "$Y")))

(define unbounded-results (cog-execute! unbounded-range))
; (format #t "The unbounded results are ~A\n" unbounded-results)
(test-assert "range unbounded size"
	(equal? 2 (length (cog-value->list unbounded-results))))

; -------------------------------------------------------------

(test-end tname)
(opencog-test-end)
