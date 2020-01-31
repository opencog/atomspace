;
; unordered-odo-equpr.scm
;
; Multiple unordered links, arranged in series (so that the odometer
; runs) but then constrained so that neighboring terms must be
; identical.  This sharply limits the possible orderings.
; See also unordered-odo-couple.scm for same test case, but with
; direct constraints.  See unordered-odo-equal.scm for the same test,
; but with virtual EqualLink.
;
(use-modules (opencog) (opencog exec))

(Evaluation (Predicate "equal") (List (Concept "L") (Concept "L")))
(Evaluation (Predicate "equal") (List (Concept "M") (Concept "M")))
(Evaluation (Predicate "equal") (List (Concept "N") (Concept "N")))
(Evaluation (Predicate "equal") (List (Concept "P") (Concept "P")))
(Evaluation (Predicate "equal") (List (Concept "Q") (Concept "Q")))
(Evaluation (Predicate "equal") (List (Concept "R") (Concept "R")))
(Evaluation (Predicate "equal") (List (Concept "S") (Concept "S")))
(Evaluation (Predicate "equal") (List (Concept "T") (Concept "T")))
(Evaluation (Predicate "equal") (List (Concept "U") (Concept "U")))
(Evaluation (Predicate "equal") (List (Concept "V") (Concept "V")))
(Evaluation (Predicate "equal") (List (Concept "W") (Concept "W")))
(Evaluation (Predicate "equal") (List (Concept "X") (Concept "X")))
(Evaluation (Predicate "equal") (List (Concept "Y") (Concept "Y")))
(Evaluation (Predicate "equal") (List (Concept "Z") (Concept "Z")))

; ----------------------------------------------------
; Coupled sets, expect 2! * 2! = 4 permutations

(List (Concept "B")
	(Set (Concept "P") (Concept "Q") (Concept "R"))
	(Set (Concept "R") (Concept "S") (Concept "T")))

(define epr-dim-two
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Present (Evaluation (Predicate "equal")
				(List (Variable "$W") (Variable "$X")))))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! epr-dim-two)

; ----------------------------------------------------
; Like above, but 2! * 1! * 2! = 4 permutations

(List (Concept "C")
	(Set (Concept "P") (Concept "Q") (Concept "R"))
	(Set (Concept "R") (Concept "S") (Concept "T"))
	(Set (Concept "T") (Concept "U") (Concept "V")))

(define epr-dim-three
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$A") (Variable "$B") (Variable "$C"))
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Present (Evaluation (Predicate "equal")
				(List (Variable "$C") (Variable "$U"))))
			(Present (Evaluation (Predicate "equal")
				(List (Variable "$W") (Variable "$X")))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! epr-dim-three)

; ----------------------------------------------------
; Like above, but 2*1*1*2 = 4 permutations

(List (Concept "D")
   (Set (Concept "P") (Concept "Q") (Concept "R"))
   (Set (Concept "R") (Concept "S") (Concept "T"))
   (Set (Concept "T") (Concept "U") (Concept "V"))
   (Set (Concept "V") (Concept "W") (Concept "X")))

(define epr-dim-four
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$A") (Variable "$B") (Variable "$C"))
				(Set (Variable "$D") (Variable "$E") (Variable "$F"))
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Present (Evaluation (Predicate "equal")
				(List (Variable "$C") (Variable "$D"))))
			(Present (Evaluation (Predicate "equal")
				(List (Variable "$F") (Variable "$U"))))
			(Present (Evaluation (Predicate "equal")
				(List (Variable "$W") (Variable "$X")))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! epr-dim-four)

; ----------------------------------------------------
