;
; unordered-odo-equal.scm
;
; Multiple unordered links, arranged in series (so that the odometer
; runs) but then constrained so that neighboring terms must be
; identical.  This sharply limits the possible orderings.
; See also unordered-odo-couple.scm for same test case, but with
; direct constraints. See also unordered-odo-equpr.scm for the
; same test, but with equality predicates.
;
(use-modules (opencog) (opencog exec))

; ----------------------------------------------------
; Coupled sets, expect 2! * 2! = 4 permutations

(List (Concept "B")
	(Set (Concept "P") (Concept "Q") (Concept "R"))
	(Set (Concept "R") (Concept "S") (Concept "T")))

(define equ-dim-two
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Equal (Variable "$W") (Variable "$X")))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! equ-dim-two)

; ----------------------------------------------------
; Like above, but 2! * 1! * 2! = 4 permutations

(List (Concept "C")
	(Set (Concept "P") (Concept "Q") (Concept "R"))
	(Set (Concept "R") (Concept "S") (Concept "T"))
	(Set (Concept "T") (Concept "U") (Concept "V")))

(define equ-dim-three
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$A") (Variable "$B") (Variable "$C"))
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Equal (Variable "$C") (Variable "$U"))
			(Equal (Variable "$W") (Variable "$X")))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! equ-dim-three)

; ----------------------------------------------------
; Like above, but 2*1*1*2 = 4 permutations

(List (Concept "D")
   (Set (Predicate "P") (Predicate "Q") (Predicate "R"))
   (Set (Predicate "R") (Predicate "S") (Predicate "T"))
   (Set (Predicate "T") (Predicate "U") (Predicate "V"))
   (Set (Predicate "V") (Predicate "W") (Predicate "X")))

(define equ-dim-four
	(Bind
		(And
			(Present (List (Variable "$CPT")
				(Set (Variable "$A") (Variable "$B") (Variable "$C"))
				(Set (Variable "$D") (Variable "$E") (Variable "$F"))
				(Set (Variable "$U") (Variable "$V") (Variable "$W"))
				(Set (Variable "$X") (Variable "$Y") (Variable "$Z"))))
			(Equal (Variable "$C") (Variable "$D"))
			(Equal (Variable "$F") (Variable "$U"))
			(Equal (Variable "$W") (Variable "$X")))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y") (Variable "$Z"))))

; (cog-execute! equ-dim-four)

; ----------------------------------------------------
