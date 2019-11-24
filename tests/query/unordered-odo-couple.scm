;
; unordered-odo-couple.scm
;
; Multiple unordered links, arranged in series (so that the odometer
; runs) but then constrained so that neighboring terms must be
; identical.  This sharply limits the possible orderings.
; See also unordered-odo-equal.scm for same test case, but with
; equality constraints.
;
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

; (cog-execute! couple-dim-two)

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

; (cog-execute! couple-dim-three)

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

; (cog-execute! couple-dim-four)

; ----------------------------------------------------
