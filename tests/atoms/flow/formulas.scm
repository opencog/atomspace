;
; formulas.scm -- Declaring formulas that compute truth values.
;
; This is a modified copy of an example program. It helps verify that
; the example actually works.
;
(use-modules (opencog) (opencog exec))

(define tvkey (Predicate "*-TruthValueKey-*"))

(define atom-a (Concept "A"))
(define atom-b (Concept "B"))
(cog-set-value! atom-a tvkey (FloatValue  0.8 1.0))
(cog-set-value! atom-b tvkey (FloatValue  0.6 0.9))

(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))
(define (confidence-of ATOM) (ElementOf (Number 1) (ValueOf ATOM tvkey)))

; Multiply the strength of the TV's of two atoms.
(define prod
	(Times (strength-of (Concept "A")) (strength-of (Concept "B"))))

(define formula-stv
	(FloatColumn
		(Minus
			(Number 1)
			(Times (strength-of (Concept "A")) (strength-of (Concept "B"))))
		(Times (confidence-of (Concept "A")) (confidence-of (Concept "B")))))

; Formula with variables
(define my-formula
   (Lambda
      (VariableList (Variable "$X") (Variable "$Y"))
		; Compute TV = (1-sA*sB, cA*cB)
		(FloatColumn
			(Minus
				(Number 1)
				(Times
					(strength-of (Variable "$X"))
					(strength-of (Variable "$Y"))))
			(Times
				(confidence-of (Variable "$X"))
				(confidence-of (Variable "$Y"))))))

(define eval-formula
	(ExecutionOutput my-formula (List (Concept "A") (Concept "B"))))

; One can also use DefinedPredicates, to give the formula a name.
(DefineLink (DefinedProcedure "has a reddish color") my-formula)

(cog-set-value! (Concept "A") tvkey (FloatValue 0.9 0.98))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.9 0.98))

; The will cause the formula to evaluate.
(define red-form
	(ExecutionOutput
		(DefinedProcedure "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; --------------------------------------------------

(cog-set-value! atom-a tvkey (FloatValue 0.8 1.0))
(cog-set-value! atom-b tvkey (FloatValue 0.6 0.9))
(define atom-c (Concept "C"))

(define key (Predicate "key"))

(define iab (Inheritance atom-a atom-b))
(define ibc (Inheritance atom-b atom-c))
(cog-set-value! iab tvkey (FloatValue  0.8 0.8))
(cog-set-value! ibc tvkey (FloatValue  0.3 0.3))

(cog-set-value! iab key (FloatValue 1 2 3))
(cog-set-value! ibc key (FloatValue 4 5 6))

; The InheritanceLink is not necessarily in any atomspace.
(Define
	(DefinedPredicate "its-about-one")
	(Lambda
		(VariableList (Variable "$x") (Variable "$y"))
		(SequentialAnd
			(GreaterThan
				(FloatValueOf (Inheritance (Variable "$x") (Variable "$y"))  key)
				(Number 0.99))
			(GreaterThan
				(Number 1.01)
				(FloatValueOf (Inheritance (Variable "$x") (Variable "$y")) key))
		)))

; Expect (its-one atom-a atom-b) to be true,
; and (its-one atom-b atom-c) to be false.
(define (its-one a b)
	(Evaluation (DefinedPredicate "its-about-one") (List a b)))

(Define
	(DefinedPredicate "mostly-confident")
	(Lambda
		(VariableList (Variable "$x") (Variable "$y"))
		(SequentialAnd
			(GreaterThan
				(confidence-of (Inheritance (Variable "$x") (Variable "$y")))
				(Number 0.75))
			(GreaterThan
				(Number 0.85)
				(confidence-of (Inheritance (Variable "$x") (Variable "$y"))))
		)))

; Expect (its-conf atom-a atom-b) to be true,
; and (its-conf atom-b atom-c) to be false.
(define (its-conf a b)
	(Evaluation (DefinedPredicate "mostly-confident") (List a b)))

; -------------------------------------------------
; Testing defined predicate execution (issue #2312).

; Initialize count to zero
(State (Anchor "sum") (Number 0))

; Define increment "function"
(Define
	(DefinedPredicate "inc")
	(True
		(Put
			(State (Anchor "sum") (Variable "$x"))
			(Plus (Number 1) (Meet (State (Anchor "sum") (Variable "$y")))))))

; MeetLink returns a UnisetValue. Unwrap it to get the NumberNode.
(define (get-sum)
	(cog-value-ref (cog-execute!
		(Meet (State (Anchor "sum") (Variable "$x"))))
		0))

(*unspecified*)
