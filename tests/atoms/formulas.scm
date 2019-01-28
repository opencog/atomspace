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
; EvaluationLink. And tehn we play a little trick.
; (define evelnk (cog-outgoing-atom the-put-result 0))

; Change the truth value on the two concept nodes ...
; (Concept "A" (stv 0.3 0.5))
; (Concept "B" (stv 0.4 0.5))

; Re-evaluate the EvaluationLink. Note the TV has been updated!
; (cog-evaluate! evelnk)

; Do it again, for good luck!
; (Concept "A" (stv 0.1 0.99))
; (Concept "B" (stv 0.1 0.99))

; Re-evaluate the EvaluationLink. The TV is again recomputed!
; (cog-evaluate! evelnk)
