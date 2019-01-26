;
; formulas.scm -- Declaring formulas that compute truth values.
;
; The rule engine, PLN and other subsystems need to be able to compute
; and alter truth values. If those formulas are known a-priori, then
; thay can be hard-coded into GroundedPredicateNodes.  However, it is
; possible that those formulas are not yet known: that they will be
; learned using some learning algorithm, for example, MOSES, or maybe
; the Pattern Miner, or some neural network.
;
; In this case, the formulas need to be placed where they can be
; accessed during computation: in the AtomSpace. The example below
; shows how formulas can be declared in the AtomSpace, but in such a
; way that they can also be evaluated to yeild an actual truth value,
; which is then attached to some Atom.
;
(use-modules (opencog) (opencog exec))

; The StrengthOfLink returns a single floating-point number,
; the strength of a TruthValue.
(cog-execute! (StrengthOf (Concept "A" (stv 0.8 1.0))))

; The demo needs at least one more Atom.
(Concept "B" (stv 0.6 0.9))

; Multiple the strength of the TV's of two atoms.
(cog-execute!
	(Times (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))

; Create a SimpleTruthValue with a non-trivial formula:
; It will be the TV := (1-sA*sB, cA*cB) where sA and sB are strenghts
; and cA, cB are confidence values. The PredicateFormulaLink assembles
; two floating-point values, and create a SimpleTruthValue out of them.
;
(cog-evaluate!
	(PredicateFormula
		(Minus
			(Number 1)
			(Times (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))
		(Times (ConfidenceOf (Concept "A")) (ConfidenceOf (Concept "B")))))

; The values do not need to be formulas; tehy can be hard-coded numbers.
(cog-evaluate!
	(PredicateFormula (Number 0.7) (Number 0.314)))

; The below computes a truth value, and attaches it to the
; EvaluationLink.
(cog-evaluate!
	(Evaluation
		(PredicateFormula (Number 0.7) (Number 0.314))
		(List
			(Concept "A")
			(Concept "B"))))

; More typically, one wishes to have a formula in the abstract,
; with variables in it, so that one can apply it in any one of
; a number of different situations. In the below, the variables
; are automatically reduced with the Atoms in the ListLink, and
; then the formula is evaluated to obtain a TruthValue.
(cog-evaluate!
	(Evaluation
		; Compute TV = (1-sA*sB, cA*cB)
		(PredicateFormula
			(Minus
				(Number 1)
				(Times
					(StrengthOf (Variable "$X"))
					(StrengthOf (Variable "$Y"))))
			(Times
				(ConfidenceOf (Variable "$X"))
				(ConfidenceOf (Variable "$Y"))))
		(List
			(Concept "A")
			(Concept "B"))))

; Optionally, you can wrap formulas with LambdaLinks. This doesn't
; really change anything; formulas work fine without LambdaLinks.
(cog-evaluate!
	(Evaluation
		; Compute TV = (1-sA*sB, cA*cB)
		(PredicateFormula
			(Lambda (Minus
				(Number 1)
				(Times
					(StrengthOf (Variable "$X"))
					(StrengthOf (Variable "$Y")))))
			(Lambda (Times
				(ConfidenceOf (Variable "$X"))
				(ConfidenceOf (Variable "$Y")))))
		(List
			(Concept "A")
			(Concept "B"))))
