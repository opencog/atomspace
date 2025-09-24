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

; The two component of a FloatValue can be accessed with
; the ElementOfLink:
(cog-set-value! (Concept "A") tvkey (FloatValue 0.8 1.0))
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
(cog-set-value! (Concept "B") tvkey (FloatValue 0.6 0.9))

; Multiply the strength of the TV's of two atoms.
(cog-execute!
	(Times (strength-of (Concept "A")) (strength-of (Concept "B"))))

; Create a FloatValue with a non-trivial formula:
; It will be the TV := (1-sA*sB, cA*cB) where sA and sB are strengths
; and cA, cB are confidence values. The FloatColumn assembles two
; floating-point values, and creates a FloatValue out of them.
;
(cog-execute!
	(FloatColumn
		(Minus
			(Number 1)
			(Times (strength-of (Concept "A")) (strength-of (Concept "B"))))
		(Times (confidence-of (Concept "A")) (confidence-of (Concept "B")))))

; The values do not need to be formulas; they can be hard-coded numbers.
(cog-execute!
	(FloatColumn (Number 0.7) (Number 0.314)))

; Typically, one wishes to have a formula with variables in it, so that
; one can apply it anywhere. The standard way of doing this is to use
; a LambdaLink to declare variable bindings.

; First, define a formula.
(define my-formula
	(Lambda
		(VariableList (Variable "$X") (Variable "$Y"))

		; Compute TV = (1-sA*sB, cA*cB)
		; This is the prototypical PLN TruthValue formula.
		(FloatColumn
			(Minus
				(Number 1)
				(Times
					(strength-of (Variable "$X"))
					(strength-of (Variable "$Y"))))
			(Times
				(confidence-of (Variable "$X"))
				(confidence-of (Variable "$Y"))))))

; Now run the formula, by applying it to some arguments.
(cog-execute!
	(ExecutionOutput my-formula (List (Concept "A") (Concept "B"))))

; Change the truth value on the two concept nodes ...
(cog-set-value! (Concept "A") tvkey (FloatValue 0.3 0.5))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.4 0.5))

; Rerun the formula... The computed value changes...
(cog-execute!
	(ExecutionOutput my-formula (List (Concept "A") (Concept "B"))))

; Do it again, for good luck!
(cog-set-value! (Concept "A") tvkey (FloatValue 0.1 0.99))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.1 0.99))

(cog-execute!
	(ExecutionOutput my-formula (List (Concept "A") (Concept "B"))))

; One can also use DefinedProcedure, to give the formula a name.
(DefineLink (DefinedProcedure "has a reddish color") my-formula)

(cog-set-value! (Concept "A") tvkey (FloatValue 0.9 0.98))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.9 0.98))

; The will cause the formula to evaluate.
(cog-execute!
	(ExecutionOutput
		(DefinedProcedure "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; The End. That's All, Folks!
; -------------------------------------------------------------------
