;
; unordered-layer.scm
;
; Unordered terms, arranged in series, inside an unordered link.
; This resembles the simpler unordered-odometer.scm except that
; here, they are layered.

(use-modules (opencog) (opencog exec))

(Set (Concept "A")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R")))

; Expect 3!=6 solutions
(define layer-dim-one
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! layer-dim-one)

; ----------------------------------------------------
; Like above, but 2!* (3! * 3!) = 72 permutations

(Set (Concept "B")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U")))

(define layer-dim-two
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! layer-dim-two)

; ----------------------------------------------------
; Like above, but 3! * (6*6*6) = 1296 permutations
(Set (Concept "C")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Predicate "V") (Predicate "W") (Predicate "X")))

(define layer-dim-three
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! layer-dim-three)

; ----------------------------------------------------
; Like above, but 4! * (6*6*6*6) = 24 * 1296 = 31104 permutations
(Set (Concept "D")
	(Set (Predicate "L") (Predicate "M") (Predicate "N"))
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "S") (Predicate "T") (Predicate "U"))
	(Set (Predicate "V") (Predicate "W") (Predicate "X")))

(define layer-dim-four
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$D") (Variable "$E") (Variable "$F"))
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! layer-dim-four)

; ----------------------------------------------------
