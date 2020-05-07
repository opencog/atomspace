;
; Verify that LambdaLink is handed properly, per discussion
; on mailing list.

(use-modules (opencog) (opencog exec))

(State (Concept "stop light") (Concept "red light"))

(define is-red-light
	(Evaluation 
		(Lambda
			(VariableList (Variable "a") (Variable "b"))
			(Equal
				(Set (Variable "a"))
				(Get (Variable "x") (State (Variable "b") (Variable "x")))))
		(List (Concept "red light") (Concept "stop light"))))

(define meet-is-red-light
	(Evaluation 
		(Lambda
			(VariableList (Variable "a") (Variable "b"))
			(Equal
				(Variable "a")
				(Meet (Variable "x") (State (Variable "b") (Variable "x")))))
		(List (Concept "red light") (Concept "stop light"))))
