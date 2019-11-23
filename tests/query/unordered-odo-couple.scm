(use-modules (opencog) (opencog exec))

; ----------------------------------------------------
; Coupled sets, expect 2! * 2! = 4 permutations

(List (Concept "B")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "R") (Predicate "S") (Predicate "T")))

(define couple-dim-two
	(Bind
		(Present (List (Variable "$CPT")
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$W") (Variable "$X") (Variable "$Y"))))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y"))))

; ----------------------------------------------------
; Like above, but 2! * 1! * 2! = 4 permutations
(List (Concept "C")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "R") (Predicate "S") (Predicate "T"))
	(Set (Predicate "T") (Predicate "U") (Predicate "V")))

(define couple-dim-three
	(Bind
		(Present (List (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$C") (Variable "$D") (Variable "$E"))
			(Set (Variable "$E") (Variable "$F") (Variable "$G"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$G"))))

; ----------------------------------------------------
; Like above, but 2*1*1*2 = 4 permutations
(List (Concept "D")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "R") (Predicate "S") (Predicate "T"))
	(Set (Predicate "T") (Predicate "U") (Predicate "V"))
	(Set (Predicate "V") (Predicate "W") (Predicate "X")))

(define couple-dim-four
	(Bind
		(Present (List (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$C") (Variable "$D") (Variable "$E"))
			(Set (Variable "$E") (Variable "$F") (Variable "$G"))
			(Set (Variable "$G") (Variable "$H") (Variable "$J"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$G") (Variable "$H") (Variable "$J"))))
