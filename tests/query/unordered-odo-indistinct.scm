;
; unordered-odo-indistinct.scm
;
; Unordered terms, arranged in series, inside an ordered link.
; Since they are in series, they force the "odometer" to run,
; during search. Similar to `unordered-odo-below.scm`, except
; that the search space is less constrained.

(use-modules (opencog) (opencog exec))

(Set (Concept "A")
	(Set (Concept "A") (Predicate "P") (Predicate "Q") (Predicate "R")))

; Expect 3!=6 solutions
(define indistinct-dim-one
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Concept "A") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! indistinct-dim-one)

; ----------------------------------------------------
; Like above, but (3! * 3!) x2  = 72 permutations

(Set (Concept "B")
	(Set (Concept "B") (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Concept "B") (Predicate "S") (Predicate "T") (Predicate "U")))

(define indistinct-dim-two
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Concept "B") (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Concept "B") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! indistinct-dim-two)

; ----------------------------------------------------
; Like above, but (6*6*6) x 3! = 1296 permutations
(Set (Concept "C")
	(Set (Concept "C") (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Concept "C") (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Concept "C") (Predicate "V") (Predicate "W") (Predicate "X")))

(define indistinct-dim-three
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Concept "C") (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Concept "C") (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Concept "C") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! indistinct-dim-three)

; ----------------------------------------------------
; Like above, but (6*6*6*6) x 4! = 31104 permutations
(Set (Concept "D")
	(Set (Concept "D") (Predicate "L") (Predicate "M") (Predicate "N"))
	(Set (Concept "D") (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Concept "D") (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Concept "D") (Predicate "V") (Predicate "W") (Predicate "X")))

(define indistinct-dim-four
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Concept "D") (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Concept "D") (Variable "$D") (Variable "$E") (Variable "$F"))
			(Set (Concept "D") (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Concept "D") (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! indistinct-dim-four)

; ----------------------------------------------------
