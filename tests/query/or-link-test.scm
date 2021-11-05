;
; or-link-test.scm -- Verify that OrLink produces sums during search.
; Reflects the discussion in issue opencog/atomspace#2644

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "or-link-test")
(test-begin tname)

; Initial data. Note the (stv 1 1) is necessary, because the IsTrueLink
; will fail with the default TruthValue of (stv 1 0) (true but not
; confident).
(State (Concept "you") (Concept "thirsty"))
(State (Concept "me") (Concept "hungry"))
(Evaluation (stv 1 1) (Predicate "cold") (Concept "me"))
(Evaluation (stv 0.6 0.1) (Predicate "tired") (Concept "her"))

(define qr2
	(Get (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "hungry")))
			(Present (State (Variable "someone") (Concept "thirsty"))))))

(test-assert "hungry or thirsty"
	(equal? (cog-execute! qr2) (Set (Concept "you") (Concept "me"))))

; ------------
; As above, but with EvaulationLink
(define qr4
	(Get (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(And
				(Present (Evaluation (Predicate "cold") (Variable "someone")))
				(IsTrue (Evaluation (Predicate "cold") (Variable "someone")))))))


(test-assert "thirsty or cold"
	(equal? (cog-execute! qr4) (Set (Concept "you") (Concept "me"))))

; ------------
; Same as above, but with implcit PresentLink
(define qr5
	(Get (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(And
				(IsTrue (Evaluation (Predicate "cold") (Variable "someone")))))))

(test-assert "thirsty or cold"
	(equal? (cog-execute! qr5) (Set (Concept "you") (Concept "me"))))

; ------------
(define qr6
	(Get (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(IsTrue (Evaluation (Predicate "cold") (Variable "someone")))
			(IsTrue (Evaluation (Predicate "tired") (Variable "someone"))))))

(test-assert "thirsty or cold but not tired"
	(equal? (cog-execute! qr6) (Set (Concept "you") (Concept "me"))))

; ------------
(define qr7
	(Get (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(GreaterThan (StrengthOf
					(Evaluation (Predicate "cold") (Variable "someone")))
				(Number 0.5))
			(GreaterThan (StrengthOf
					(Evaluation (Predicate "tired") (Variable "someone")))
				(Number 0.5)))))

(test-assert "strong tired no confidence"
	(equal? (cog-execute! qr7)
		(Set (Concept "you") (Concept "me") (Concept "her"))))

(define qr8
	(Get (TypedVariable (Variable "someone") (Type 'Concept))
		(Or
			(Present (State (Variable "someone") (Concept "thirsty")))
			(Not (GreaterThan (Number 0.5) (StrengthOf
				(Evaluation (Predicate "cold") (Variable "someone")))))
			(Not (GreaterThan (Number 0.5) (StrengthOf
				(Evaluation (Predicate "tired") (Variable "someone"))))))))

(test-assert "not strong tired no confidence"
	(equal? (cog-execute! qr8)
		(Set (Concept "you") (Concept "me") (Concept "her"))))

; ------------
; Add the stv to force it to be strictly true.
(Evaluation (stv 1 1) (Predicate "tired") (Concept "her"))

(test-assert "thirsty or cold or tired"
	(equal? (cog-execute! qr6)
		(Set (Concept "you") (Concept "me") (Concept "her"))))

(test-end tname)
