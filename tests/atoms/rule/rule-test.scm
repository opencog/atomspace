;
; rule-test.scm - Test the basic RuleLink syntax.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "rule-test")
(test-begin tname)

; ---------------------------------------------------------------
; Define two rules. These will be the two rules of minimal implication
; logic, written in the natural deduction style. (We ignore that the
; AtomSpace means that all rules are always Gentzen-like, and just
; pretend we are doing plain natural deduction.) Well make things
; interesting by using typed variables.
;
; First, implication introduction:
; If B is true, then A->B (anything proves B).
(define intro
	(Rule
		; Typed variables appearing in the terms
		(VariableList
			(TypedVariable (Variable "$A") (Type 'ConceptNode))
			(TypedVariable (Variable "$B") (Type 'ConceptNode)))

		; Assumption aka premise
		(Variable "$B")

		; Deduction aka conclusion
		(Implication (Variable "$A") (Variable "$B"))))

; Next, implication elimination aka Modus Ponens:
; If A->B and A is true, then B.
(define elim
	(Rule
		; Typed variables appearing in the terms
		(VariableList
			(TypedVariable (Variable "$A") (Type 'ConceptNode))
			(TypedVariable (Variable "$B") (Type 'ConceptNode)))

		; Assumptions (list of premises)
		(SequentialAnd
			(Implication (Variable "$A") (Variable "$B"))
			(Variable "$A"))

		; Deduction aka conclusion
		(Variable "$B")))

; ---------------------------------------------------------------
; Lets dissect these two rules into thier parts, and make sure that
; the parts are as expected:

(define var-ab
	(VariableList
		(TypedVariable (Variable "$A") (Type 'ConceptNode))
		(TypedVariable (Variable "$B") (Type 'ConceptNode))))

(define lamb-b
	(Lambda
		(TypedVariable (Variable "$B") (Type 'ConceptNode))
		(Variable "$B")))

(define lamb-impl
	(Lambda
		(VariableList
			(TypedVariable (Variable "$A") (Type 'ConceptNode))
			(TypedVariable (Variable "$B") (Type 'ConceptNode)))
		(Implication
			(Variable "$A")
			(Variable "$B"))))

(test-assert "vardecl intro" (equal? (cog-execute! (VardeclOf intro)) var-ab))
(test-assert "premise intro" (equal? (cog-execute! (PremiseOf intro)) lamb-b))
(test-assert "conclud intro" (equal? (cog-execute! (ConclusionOf intro)) lamb-impl))

(test-assert "vardecl elim" (equal? (cog-execute! (VardeclOf elim)) var-ab))
(test-assert "premi-1 elim" (equal? (cog-execute! (PremiseOf elim (Number 0))) lamb-impl))
(test-assert "premi-2 elim" (equal? (cog-execute! (PremiseOf elim (Number 1))) lamb-b))
(test-assert "conclud elim" (equal? (cog-execute! (ConclusionOf elim)) lamb-b))

(test-end tname)
(opencog-test-end)
