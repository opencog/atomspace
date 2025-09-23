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

(define tvkey (Predicate "*-TruthValueKey-*"))

; The two component of a SimpleTruthValue can be accessed with
; the ElementOfLink:
(cog-set-value! (Concept "A") tvkey (SimpleTruthValue 0.8 1.0))
(cog-execute! (ElementOf (Number 0) (ValueOf (Concept "A") tvkey)))
(cog-execute! (ElementOf (Number 1) (ValueOf (Concept "A") tvkey)))

; The above Atomese is verbose. That's just how Atomese is. To make
; the demo easier, its handy to write a bit of scheme to keep focus
; on what we want to show, rather than how verbose Atomese is...
(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))
(define (confidence-of ATOM) (ElementOf (Number 1) (ValueOf ATOM tvkey)))

; Double-check that this works:
(cog-execute! (strength-of (Concept "A")))
(cog-execute! (confidence-of (Concept "A")))

; The demo needs at least one more Atom.
(cog-set-value! (Concept "B") tvkey (SimpleTruthValue 0.6 0.9))

; Multiply the strength of the TV's of two atoms.
(cog-execute!
	(Times (strength-of (Concept "A")) (strength-of (Concept "B"))))

; Create a SimpleTruthValue with a non-trivial formula:
; It will be the TV := (1-sA*sB, cA*cB) where sA and sB are strengths
; and cA, cB are confidence values. The FormulaPredicateLink assembles
; two floating-point values, and creates a SimpleTruthValue out of them.
;
(cog-execute!
	(FormulaPredicate
		(Minus
			(Number 1)
			(Times (strength-of (Concept "A")) (strength-of (Concept "B"))))
		(Times (confidence-of (Concept "A")) (confidence-of (Concept "B")))))

; The values do not need to be formulas; they can be hard-coded numbers.
(cog-execute!
	(FormulaPredicate (Number 0.7) (Number 0.314)))

; The below computes a truth value, and attaches it to the
; EvaluationLink. Let's demo this in three parts: first define it,
; then evaluate it, then look at it.
;
(define my-ev-link
	(Evaluation
		(FormulaPredicate (Number 0.7) (Number 0.314))
		(List
			(Concept "A")
			(Concept "B"))))

; Evaluate ...
(cog-execute! my-ev-link)

; Print.
(display my-ev-link)

; More typically, one wishes to have a formula in the abstract,
; with variables in it, so that one can apply it in any one of
; a number of different situations. In the below, the variables
; are automatically reduced with the Atoms in the ListLink, and
; then the formula is evaluated to obtain a TruthValue.
(cog-execute!
	(Evaluation
		; Compute TV = (1-sA*sB, cA*cB)
		(FormulaPredicate
			(Minus
				(Number 1)
				(Times
					(strength-of (Variable "$X"))
					(strength-of (Variable "$Y"))))
			(Times
				(confidence-of (Variable "$X"))
				(confidence-of (Variable "$Y"))))
		(List
			(Concept "A")
			(Concept "B"))))

; Optionally, you can wrap formulas with LambdaLinks. This doesn't
; really change anything; formulas work fine without LambdaLinks.
(cog-execute!
	(Evaluation
		; Compute TV = (1-sA*sB, cA*cB)
		(FormulaPredicate
			(Lambda (Minus
				(Number 1)
				(Times
					(strength-of (Variable "$X"))
					(strength-of (Variable "$Y")))))
			(Lambda (Times
				(confidence-of (Variable "$X"))
				(confidence-of (Variable "$Y")))))
		(List
			(Concept "A")
			(Concept "B"))))


; The FormulaPredicateLink behaves just like any other algebraic
; expression with VariableNodes in it. When executed, it might
; reduce a bit, but that is all.
(cog-execute!
	(FormulaPredicate
		(Plus (Number 41)
			(Minus
				(Number 1)
				(Times
					(strength-of (Variable "$VA"))
					(strength-of (Variable "$VB")))))
			(Times
				(confidence-of (Variable "$VA"))
				(confidence-of (Variable "$VB")))))

; Beta-reduction works as normal. The below will create an
; EvaluationLink with ConceptNode A and B in it, and will set the
; truth value according to the formula.
(define the-put-result
	(cog-execute!
		(PutLink
			(VariableList (Variable "$VA") (Variable "$VB"))
			(Evaluation
				; Compute TV = (1-sA*sB, cA*cB)
				(FormulaPredicate
					(Minus
						(Number 1)
						(Times
							(strength-of (Variable "$VA"))
							(strength-of (Variable "$VB"))))
					(Times
						(confidence-of (Variable "$VA"))
						(confidence-of (Variable "$VB"))))
				(List
					(Variable "$VA") (Variable "$VB")))
		(Set (List (Concept "A") (Concept "B"))))))

; The scheme variable `the-put-result` contains a SetLink with the
; result in it. Lets unwrap it, so that `evelnk` is just the
; EvaluationLink. And then we play a little trick.
(define evelnk (cog-outgoing-atom the-put-result 0))

; Change the truth value on the two concept nodes ...
(cog-set-value! (Concept "A") tvkey (SimpleTruthValue 0.3 0.5))
(cog-set-value! (Concept "B") tvkey (SimpleTruthValue 0.4 0.5))

; Re-evaluate the EvaluationLink. Note the TV has been updated!
(cog-execute! evelnk)

; Do it again, for good luck!
(cog-set-value! (Concept "A") tvkey (SimpleTruthValue 0.1 0.99))
(cog-set-value! (Concept "B") tvkey (SimpleTruthValue 0.1 0.99))

; Re-evaluate the EvaluationLink. The TV is again recomputed!
(cog-execute! evelnk)

; One can also use DefinedPredicates, to give the formula a name.
(DefineLink
	(DefinedPredicate "has a reddish color")
	(FormulaPredicate
		(Minus
			(Number 1)
			(Times
				(strength-of (Variable "$X"))
				(strength-of (Variable "$Y"))))
		(Times
			(confidence-of (Variable "$X"))
			(confidence-of (Variable "$Y")))))

(cog-set-value! (Concept "A") tvkey (SimpleTruthValue 0.9 0.98))
(cog-set-value! (Concept "B") tvkey (SimpleTruthValue 0.9 0.98))

; The will cause the formula to evaluate.
(cog-execute!
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List
			(Concept "A")
			(Concept "B"))))

; The End. That's All, Folks!
; -------------------------------------------------------------------
