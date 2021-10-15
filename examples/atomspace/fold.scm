
(use-modules (opencog) (opencog exec))

(define a-set
	(LinkValue
		(Concept "A" (CountTruthValue 1 0 1))
		(Concept "B" (CountTruthValue 1 0 2))
		(Concept "C" (CountTruthValue 1 0 3))))

(cog-prt-atomspace)

(cog-execute! (CountOf a-set))

;
