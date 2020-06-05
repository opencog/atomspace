;
; choice-present.scm
;
; Validate PresentLinks appearing in ChoiceLinks
; Per issue #2644

(use-modules (opencog) (opencog exec))

(State (List (Concept "alice") (Predicate "hungry")) (Concept "TRUE"))
(State (Concept "alice") (Concept "at home"))

(define who-is-hungry?
	(Get
		(VariableList
			(TypedVariable (Variable "x") (Type "ConceptNode"))
			(TypedVariable (Variable "y") (Type "ConceptNode")))
		(Choice
			(Present
				(State (Variable "x") (Variable "y"))
				(State (List (Variable "x") (Predicate "hungry")) (Concept "TRUE"))
			))))

; (cog-execute! who-is-hungry?)

; Above should result in this:
(define expected (Set (List (Concept "alice") (Concept "at home"))))
