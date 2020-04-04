;
; formulas.scm -- Declaring formulas that compute truth values.
;
; Arithmetic consists of addition, subtraction, multiplication and
; division; there are explicit Atom types to represent these. Thus,
; arithmetic formulas can be stored in the atomspace.
;
; Because the AtomSpace holds descriptive, declarative knowledge, these
; formulas can be manipulated and edited. For example, an algebra system
; (implemented as a collection of rules) can work with the formulas.
;
; But also, these formulas can be explicitly evaluated. The arithmetic
; operators (PlusLink, TimesLink, etc.) know how to add and multiply
; numbers and values. Thus, however a formula has been obtained (whether
; by computation, search, reasoning, algebra or learning), it can then
; be applied to (time-changing) Values. This example shows how formulas
; are used to modify TruthValues.
;
; The next example, `flows.scm`, shows how to attach such TV's to
; arbitrary Atoms.
;
(use-modules (opencog) (opencog exec))

; The StrengthOfLink returns a single floating-point number,
; the strength of a TruthValue.
(Concept "A" (stv 0.8 1.0))
(cog-execute! (StrengthOf (Concept "A")))

; The demo needs at least one more Atom.
(Concept "B" (stv 0.6 0.9))

; Multiply the strength of the TV's of two atoms.
(cog-execute!
	(Times (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))

; Create a SimpleTruthValue with a non-trivial formula:
; It will be the TV := (1-sA*sB, cA*cB) where sA and sB are strengths
; and cA, cB are confidence values. The PredicateFormulaLink assembles
; two floating-point values, and creates a SimpleTruthValue out of them.
;
(cog-evaluate!
	(PredicateFormula
		(Minus
			(Number 1)
			(Times (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))
		(Times (ConfidenceOf (Concept "A")) (ConfidenceOf (Concept "B")))))

; The values do not need to be formulas; they can be hard-coded numbers.
(cog-evaluate!
	(PredicateFormula (Number 0.7) (Number 0.314)))

; The below computes a truth value, and attaches it to the
; EvaluationLink. Let's demo this in three parts: first define it,
; then evaluate it, then look at it.
;
(define my-ev-link
	(Evaluation
		(PredicateFormula (Number 0.7) (Number 0.314))
		(List
			(Concept "A")
			(Concept "B"))))

; Evaluate ...
(cog-evaluate! my-ev-link)

; Print.
(display my-ev-link)

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


; The PredicateFormulaLink behaves just like any other algebraic
; expression with VariableNodes in it. When executed, it might
; reduce a bit, but that is all.
(cog-execute!
	(PredicateFormula
		(Plus (Number 41)
			(Minus
				(Number 1)
				(Times
					(StrengthOf (Variable "$VA"))
					(StrengthOf (Variable "$VB")))))
			(Times
				(ConfidenceOf (Variable "$VA"))
				(ConfidenceOf (Variable "$VB")))))

; Beta-reduction works as normal. The below will create an
; EvaluationLink with ConceptNode A and B in it, and will set the
; truth value according to the formula.
(define the-put-result
	(cog-execute!
		(PutLink
			(VariableList (Variable "$VA") (Variable "$VB"))
			(Evaluation
				; Compute TV = (1-sA*sB, cA*cB)
				(PredicateFormula
					(Minus
						(Number 1)
						(Times
							(StrengthOf (Variable "$VA"))
							(StrengthOf (Variable "$VB"))))
					(Times
						(ConfidenceOf (Variable "$VA"))
						(ConfidenceOf (Variable "$VB"))))
				(List
					(Variable "$VA") (Variable "$VB")))
		(Set (List (Concept "A") (Concept "B"))))))

; The scheme variable `the-put-result` contains a SetLink with the
; result in it. Lets unwrap it, so that `evelnk` is just the
; EvaluationLink. And then we play a little trick.
(define evelnk (cog-outgoing-atom the-put-result 0))

; Change the truth value on the two concept nodes ...
(Concept "A" (stv 0.3 0.5))
(Concept "B" (stv 0.4 0.5))

; Re-evaluate the EvaluationLink. Note the TV has been updated!
(cog-evaluate! evelnk)

; Do it again, for good luck!
(Concept "A" (stv 0.1 0.99))
(Concept "B" (stv 0.1 0.99))

; Re-evaluate the EvaluationLink. The TV is again recomputed!
(cog-evaluate! evelnk)

; One can also use DefinedPredicates, to give the formula a name.
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

(Concept "A" (stv 0.9 0.98))
(Concept "B" (stv 0.9 0.98))

; The will cause the formula to evaluate.
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List
			(Concept "A")
			(Concept "B"))))
