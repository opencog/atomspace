;
; unordered-odo-distinct.scm
;
; Unordered terms, arranged in series, inside an ordered link.
; Since they are in series, they force the "odometer" to run,
; during search. Similar to `unordered-odo-indistinct.scm`, except
; that the search space is more constrained.

(use-modules (opencog) (opencog exec))

(Set (Concept "A")
	(Set (Concept "A") (Predicate "P") (Predicate "Q") (Predicate "R")))

; Expect 3!=6 solutions
(define distinct-dim-one
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Concept "A") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! distinct-dim-one)

; ----------------------------------------------------
; Like above, but 3! * 3! = 36 permutations

(Set (Concept "B")
	(Set (Concept "B1") (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Concept "B2") (Predicate "S") (Predicate "T") (Predicate "U")))

(define distinct-dim-two
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Concept "B1") (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Concept "B2") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! distinct-dim-two)

; ----------------------------------------------------
; Like above, but 6*6*6 = 216 permutations
(Set (Concept "C")
	(Set (Concept "C1") (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Concept "C2") (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Concept "C3") (Predicate "V") (Predicate "W") (Predicate "X")))

(define distinct-dim-three
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Concept "C1") (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Concept "C2") (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Concept "C3") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! distinct-dim-three)

; ----------------------------------------------------
; Like above, but 6*6*6*6 = 1296 permutations
(Set (Concept "D")
	(Set (Concept "D1") (Predicate "L") (Predicate "M") (Predicate "N"))
	(Set (Concept "D2") (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Concept "D3") (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Concept "D4") (Predicate "V") (Predicate "W") (Predicate "X")))

(define distinct-dim-four
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Concept "D1") (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Concept "D2") (Variable "$D") (Variable "$E") (Variable "$F"))
			(Set (Concept "D3") (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Concept "D4") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! distinct-dim-four)

; ----------------------------------------------------
