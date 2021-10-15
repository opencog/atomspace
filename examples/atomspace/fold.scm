
(use-modules (opencog) (opencog exec))

; TODO maybe a ForEachLink to apply a function to a list or a set?
(define a-set
	(Set
		(Concept "A" (CountTruthValue 1 0 1))
		(Concept "B" (CountTruthValue 1 0 2))
		(Concept "C" (CountTruthValue 1 0 3))))

(cog-prt-atomspace)

(cog-execute! (CountOf a-set))
(cog-execute! (CountOf (ForEach a-set)))
; Perhaps ForEach returns a LinkValue that CountValue can process??

(define qry
	(Meet
		(TypedVariable (Variable "$c") (Type 'Concept))
		(Present (Variable "$c"))))

(cog-execute! qry)

;Ideas:
(cog-execute! (CountOf qry))
(cog-execute! (CountOf (Run qry)))
;
