;
; formulas.scm -- Declaring formulas that compute truth values.
;
(use-modules (opencog) (opencog exec))

; Declare a very simplified example of a truth-value formula used by
; PLN.  This merely multiplies a pair of strengths.

(cog-execute! (StrengthOf (Concept "A" (stv 0.8 1.0))))

(Concept "B" (stv 0.6 0.9))

(cog-execute! 
	(TimesLink (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))

(cog-evaluate!
	(PredicateFormulaLink
		(TimesLink (StrengthOf (Concept "A")) (StrengthOf (Concept "B")))
		(TimesLink (ConfidenceOf (Concept "A")) (ConfidenceOf (Concept "B")))))

(cog-evaluate!
	(PredicateFormulaLink (Number 0.7) (Number 0.314))

(cog-evaluate!
	(EvaluationLink
		(PredicateFormulaLink (Number 0.7) (Number 0.314))
		(List
			(Concept "A")
			(Concept "B"))))

(EvaluationLink
	(TimesLink (StrengthOf (Variable "$A") (Variable "$B")))
