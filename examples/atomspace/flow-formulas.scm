;
; flow-formulas.scm -- Dynamically changing flows.
;

(use-modules (opencog) (opencog exec))

; A formula for computing a SimpleTruthValue, based on two input Atoms.
; See the `formulas.scm` example for a detailed explanation of how
; this should be understood.
(DefineLink
   (DefinedPredicate "has a reddish color")
   (PredicateFormula
      (Minus
         (Number 1)
         (Times
            (StrengthOf (Variable "$X"))
            (StrengthOf (Variable "$Y"))))
      (Times
         (ConfidenceOf (Variable "$X"))
         (ConfidenceOf (Variable "$Y")))))

; Create an EvalationLink that will apply the formula above to a pair
; of Atoms. See the `formulas.scm` example for details.
(define evlnk
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; As in earlier examples, the TV on the EvaluationLink is recomputed
; every time that it is evaluated.
(cog-set-tv! (Concept "A") (stv 0.3 0.7))
(cog-set-tv! (Concept "B") (stv 0.4 0.6))
(cog-execute! evlnk)
(cog-tv evlnk)

(define evstream (EvaluationStream evlnk))
