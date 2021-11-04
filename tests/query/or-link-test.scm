;
; or-link-test.scm -- Verify that OrLink produces sums during search.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "or-link-space-test")
(test-begin tname)

; Initial data. Note the (stv 1 1) is necessary, because the IsTrueLink
; will fail with the default TruthValue of (stv 1 0) (true but not
; confident).
(State (Concept "you") (Concept "thirsty"))
(Evaluation (stv 1 1) (Predicate "cold") (Concept "me"))
(Evaluation (Predicate "tired") (Concept "her"))

(define qr4
	(Get (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(And
				(Present (Evaluation (Predicate "cold") (Variable "someone")))
				(IsTrue (Evaluation (Predicate "cold") (Variable "someone")))))))


; (cog-execute! qr4)
(test-assert "thirsty or cold"
	(equal? (cog-execute! qr4) (Set (ConceptNode "you") (ConceptNode "me"))))

(test-end tname)
