;
; choice-compound.scm
;
; ChoiceLink with PresentLink -- This tests the choice of compounded
; (multiple-mandatory) terms wrapped into a single alternative.
; Derived from issue https://github.com/opencog/atomspace/issues/2644

(use-modules (opencog) (opencog exec))

; Data
(State (List (Concept "Andrew") (Predicate "called robot")) (Concept "true"))

(State (List (Concept "Betty") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Betty") (Predicate "has crate")) (Concept "false"))

(State (List (Concept "Cathy") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Cathy") (Predicate "has crate")) (Concept "true"))
(State (List (Concept "Cathy") (Predicate "crate is full")) (Concept "true"))

(State (List (Concept "Deborah") (Predicate "called robot")) (Concept "false"))

(State (List (Concept "Edward") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Edward") (Predicate "has crate")) (Concept "true"))

(State (List (Concept "Frank") (Predicate "called robot")) (Concept "false"))
(State (List (Concept "Frank") (Predicate "has crate")) (Concept "true"))
(State (List (Concept "Frank") (Predicate "crate is full")) (Concept "false"))

(State (List (Concept "George") (Predicate "called robot")) (Concept "true"))
(State (List (Concept "George") (Predicate "movement")) (Concept "approaching"))

(define answer
	(Set (ConceptNode "Andrew") (ConceptNode "Betty") (ConceptNode "Cathy")))

; This poses question: who wants or needs the robot?
; This should return only Andrew, Betty, Cathy
(define who-needs-help?
	(Get
		(TypedVariable (Variable "picker") (Type "Concept"))
		(And
			;;; Not needed any more ...
			;;; (Present (Variable "picker"))
			(Absent
				(State
					(List (Variable "picker") (Predicate "movement"))
					(Concept "approaching")))
			(Choice
				(Present
					(State
						(List (Variable "picker") (Predicate "called robot"))
						(Concept "true")))
				(Present
					(State
						(List (Variable "picker") (Predicate "called robot"))
						(Concept "false"))
					(State
						(List (Variable "picker") (Predicate "has crate"))
						(Concept "false")))
				(Present
					(State
						(List (Variable "picker") (Predicate "called robot"))
						(Concept "false"))
					(State
						(List (Variable "picker") (Predicate "has crate"))
						(Concept "true"))
					(State
						(List (Variable "picker") (Predicate "crate is full"))
						(Concept "true")))
			))))

; (cog-execute! who-needs-help?)
