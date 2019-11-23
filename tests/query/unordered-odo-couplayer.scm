;
; unordered-odo-couplayer.scm
;
; Multiple unordered links, arranged in series (so that the odometer
; runs) but then constrained so that neighboring terms must be
; identical.  This sharply limits the possible orderings.
; But then, the embedding links tehmselves are unordered, allowing
; the pattern matcher to get confused...
; See also unordered-odo-couple.scm for same test case, but with
; ordered embedding links.
;
(use-modules (opencog) (opencog exec))

; ----------------------------------------------------
; Coupled sets, expect 2! * 2! = 4 permutations

(Set (Concept "B")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "R") (Predicate "S") (Predicate "T")))

(define play-dim-two
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$U") (Variable "$V") (Variable "$W"))
			(Set (Variable "$W") (Variable "$X") (Variable "$Y"))))
		(Associative
			(Variable "$U") (Variable "$V") (Variable "$W")
			(Variable "$X") (Variable "$Y"))))

; ----------------------------------------------------
; Like above, but 2! * 1! * 2! = 4 permutations
(Set (Concept "C")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "R") (Predicate "S") (Predicate "T"))
	(Set (Predicate "T") (Predicate "U") (Predicate "V")))

(define play-dim-three
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$C") (Variable "$D") (Variable "$E"))
			(Set (Variable "$E") (Variable "$F") (Variable "$G"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$G"))))

; ----------------------------------------------------
; Like above, but 2*1*1*2 = 4 permutations
(Set (Concept "D")
	(Set (Predicate "P") (Predicate "Q") (Predicate "R"))
	(Set (Predicate "R") (Predicate "S") (Predicate "T"))
	(Set (Predicate "T") (Predicate "U") (Predicate "V"))
	(Set (Predicate "V") (Predicate "W") (Predicate "X")))

(define play-dim-four
	(Bind
		(Present (Set (Variable "$CPT")
			(Set (Variable "$A") (Variable "$B") (Variable "$C"))
			(Set (Variable "$C") (Variable "$D") (Variable "$E"))
			(Set (Variable "$E") (Variable "$F") (Variable "$G"))
			(Set (Variable "$G") (Variable "$H") (Variable "$J"))))
		(Associative
			(Variable "$A") (Variable "$B") (Variable "$C")
			(Variable "$D") (Variable "$E") (Variable "$F")
			(Variable "$G") (Variable "$H") (Variable "$J"))))
