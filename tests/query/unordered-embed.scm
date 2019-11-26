;
; unordered-embed.scm
;
; Test odometer concepts, for UnorderedLinks that are deeper down
; in the search.

(use-modules (opencog) (opencog exec))

(List (Concept "A") (Set (Predicate "P") (Predicate "Q")))

(define embedded-set
	(Bind
		(Present (List (Variable "$C") (Set (Variable "$X") (Variable "$Y"))))
		(Implication (Variable "$X") (Variable "$Y"))))

; (cog-execute! embedded-set)

; The expected answer from above.
(define expect-embedded-set
	(Set
		(Implication (Predicate "P") (Predicate "Q"))
		(Implication (Predicate "Q") (Predicate "P"))))

; ----------------------------------------------------
; Like above, but four permutations

(List (Concept "B")
   (Set (Predicate "P") (Predicate "Q"))
   (Set (Predicate "R") (Predicate "S")))

(define two-x-two
	(Bind
		(Present (List (Variable "$C")
			(Set (Variable "$X") (Variable "$Y"))
			(Set (Variable "$Z") (Variable "$W"))))
		(Implication
			(Implication (Variable "$X") (Variable "$Y"))
			(Implication (Variable "$Z") (Variable "$W")))))

; (cog-execute! two-x-two)

(define expect-two-x-two
	(Set
		(ImplicationLink
			(ImplicationLink (PredicateNode "P") (PredicateNode "Q"))
			(ImplicationLink (PredicateNode "R") (PredicateNode "S")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "P") (PredicateNode "Q"))
			(ImplicationLink (PredicateNode "S") (PredicateNode "R")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "Q") (PredicateNode "P"))
			(ImplicationLink (PredicateNode "R") (PredicateNode "S")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "Q") (PredicateNode "P"))
			(ImplicationLink (PredicateNode "S") (PredicateNode "R")))
))

; ----------------------------------------------------
; Like above, but eight permutations

(List (Concept "C")
	(Set (Predicate "P") (Predicate "Q"))
	(Set (Predicate "R") (Predicate "S"))
	(Set (Predicate "T") (Predicate "U")))

(define cube
	(Bind
		(Present (List (Variable "$C")
			(Set (Variable "$U") (Variable "$V"))
			(Set (Variable "$X") (Variable "$Y"))
			(Set (Variable "$Z") (Variable "$W"))))
		(Implication
			(Implication (Variable "$U") (Variable "$V"))
			(Implication (Variable "$X") (Variable "$Y"))
			(Implication (Variable "$Z") (Variable "$W")))))

; (cog-arity (cog-execute! cube))

(define expect-cube
	(Set
		(ImplicationLink
			(ImplicationLink (PredicateNode "P") (PredicateNode "Q"))
			(ImplicationLink (PredicateNode "R") (PredicateNode "S"))
			(ImplicationLink (PredicateNode "T") (PredicateNode "U")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "P") (PredicateNode "Q"))
			(ImplicationLink (PredicateNode "S") (PredicateNode "R"))
			(ImplicationLink (PredicateNode "T") (PredicateNode "U")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "Q") (PredicateNode "P"))
			(ImplicationLink (PredicateNode "R") (PredicateNode "S"))
			(ImplicationLink (PredicateNode "T") (PredicateNode "U")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "Q") (PredicateNode "P"))
			(ImplicationLink (PredicateNode "S") (PredicateNode "R"))
			(ImplicationLink (PredicateNode "T") (PredicateNode "U")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "P") (PredicateNode "Q"))
			(ImplicationLink (PredicateNode "R") (PredicateNode "S"))
			(ImplicationLink (PredicateNode "U") (PredicateNode "T")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "P") (PredicateNode "Q"))
			(ImplicationLink (PredicateNode "S") (PredicateNode "R"))
			(ImplicationLink (PredicateNode "U") (PredicateNode "T")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "Q") (PredicateNode "P"))
			(ImplicationLink (PredicateNode "R") (PredicateNode "S"))
			(ImplicationLink (PredicateNode "U") (PredicateNode "T")))
		(ImplicationLink
			(ImplicationLink (PredicateNode "Q") (PredicateNode "P"))
			(ImplicationLink (PredicateNode "S") (PredicateNode "R"))
			(ImplicationLink (PredicateNode "U") (PredicateNode "T")))
))

; ----------------------------------------------------
; Like above, but sixteen permutations

(List (Concept "D")
	(Set (Predicate "P") (Predicate "Q"))
	(Set (Predicate "R") (Predicate "S"))
	(Set (Predicate "T") (Predicate "U"))
	(Set (Predicate "V") (Predicate "W")))

(define tesseract
	(Bind
		(Present (List (Variable "$C")
			(Set (Variable "$A") (Variable "$B"))
			(Set (Variable "$U") (Variable "$V"))
			(Set (Variable "$X") (Variable "$Y"))
			(Set (Variable "$Z") (Variable "$W"))))
		(Implication
			(Implication (Variable "$A") (Variable "$B"))
			(Implication (Variable "$U") (Variable "$V"))
			(Implication (Variable "$X") (Variable "$Y"))
			(Implication (Variable "$Z") (Variable "$W")))))

; (cog-arity (cog-execute! tesseract))

; ----------------------------------------------------
