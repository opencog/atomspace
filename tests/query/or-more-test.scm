;
; or-more-test.scm -- Verify more variations involving the OrLink.
; Reflects the discussion in issue opencog/atomspace#2644

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "or-more-test")
(test-begin tname)

; Some simple one or two clause choices.
(State (List (Concept "alice") (Predicate "hungry")) (Concept "TRUE"))
(State (Concept "alice") (Concept "at home"))

(define who-is-hungry-1?
	(Get
		(VariableList
			(TypedVariable (Variable "x") (Type "ConceptNode"))
			(TypedVariable (Variable "y") (Type "ConceptNode")))
		(Present
			(State (Variable "x") (Variable "y"))
			(State (List (Variable "x") (Predicate "hungry")) (Concept "TRUE"))
		)))

(test-assert "alice at home"
	(equal? (cog-execute! who-is-hungry-1?)
		(Set (List (Concept "alice") (Concept "at home")))))

; -----------------
(define who-is-hungry-2?
	(Get
		(VariableList
			(TypedVariable (Variable "x") (Type "ConceptNode"))
			(TypedVariable (Variable "y") (Type "ConceptNode")))
		(Or
			(Present (State (Variable "x") (Variable "y")))
			(Present (State (List (Variable "x") (Predicate "hungry")) (Concept "TRUE")))
			)))

(test-assert "alice y at home"
	(equal? (cog-execute! who-is-hungry-2?)
		(Set
			(List (Concept "alice") (Concept "at home"))
			(List (Concept "alice") (Variable "y")))))

; -----------------
(define who-is-hungry-3?
	(Get
		(VariableList
			(TypedVariable (Variable "x") (Type "ConceptNode"))
			(TypedVariable (Variable "y") (Type "ConceptNode")))
		(Choice
			(Present (State (Variable "x") (Variable "y")))
			(Present (State (List (Variable "x") (Predicate "hungry")) (Concept "TRUE")))
			)))

(test-assert "alice why at home"
	(equal? (cog-execute! who-is-hungry-3?)
		(Set
			(List (Concept "alice") (Concept "at home"))
			(List (Concept "alice") (Variable "y")))))

(test-end tname)
