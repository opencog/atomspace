;
; formulas.scm -- Declaring formulas that compute truth values.
;
; This is a modified copy of an example program. It helps verify that
; the example actually works.
;
(use-modules (opencog) (opencog exec))

(define atom-a (Concept "A" (stv 0.8 1.0)))
(define atom-b (Concept "B" (stv 0.6 0.9)))

; Multiply the strength of the TV's of two atoms.
(define prod
	(Times (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))

(define stv-const (PredicateFormula (Number 0.7) (Number 0.314)))

(define formula-stv
	(PredicateFormula
		(Minus
			(Number 1)
			(Times (StrengthOf (Concept "A")) (StrengthOf (Concept "B"))))
		(Times (ConfidenceOf (Concept "A")) (ConfidenceOf (Concept "B")))))

; The below computes a truth value, and attaches it to the
; EvaluationLink.
(define my-ev-link
	(Evaluation
		(PredicateFormula (Number 0.75) (Number 0.628))
		(List
			(Concept "A")
			(Concept "B"))))

; Formula with variables
(define eval-formula
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
(define eval-lambda
	(Evaluation
		; Compute TV = (1-sA*sB, cA*cB)
		(PredicateFormula
			(Lambda
				; Lambda without a decl, intentionally so.
				; (NopeVariableList (Variable "$X") (Variable "$Y"))
				(Minus
					(Number 1)
					(Times
						(StrengthOf (Variable "$X"))
						(StrengthOf (Variable "$Y")))))
			(Lambda
				(VariableList (Variable "$X") (Variable "$Y"))
				(Times
					(ConfidenceOf (Variable "$X"))
					(ConfidenceOf (Variable "$Y")))))
		(List
			(Concept "A")
			(Concept "B"))))


; Beta-reducation works as normal. The below will create an
; EvaluationLink with ConceptNode A and B in it, and will set the
; truth value according to the formula.
(define put-link
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
		(Set (List (Concept "A") (Concept "B")))))


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
(define red-form
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List
			(Concept "A")
			(Concept "B"))))
