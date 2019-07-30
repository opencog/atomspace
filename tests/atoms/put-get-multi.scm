(use-modules (opencog) (opencog exec))

(Inheritance (Concept "Jim") (Concept "Father"))
(Inheritance (Concept "Jane") (Concept "Mother"))

(define put-get-multi
	(PutLink
		(LambdaLink
			(VariableList (Variable "x") (Variable "y"))
			(EvaluationLink
				(PredicateNode "relatives")
				(ListLink
					(Variable "x")
					(Variable "y")
					(Concept "mom and pop"))))
		(ListLink
			(GetLink (TypedVariable (Variable "$X") (Type 'ConceptNode))
				(Inheritance (Variable "$X") (Concept "Father")))
			(GetLink (TypedVariable (Variable "$X") (Type 'ConceptNode))
				(Inheritance (Variable "$X") (Concept "Mother"))))))

(define put-get-multi-empty
	(PutLink
		(LambdaLink
			(VariableList (Variable "x") (Variable "y"))
			(EvaluationLink
				(PredicateNode "relatives")
				(ListLink
					(Variable "x")
					(Variable "y")
					(Concept "mom and pop"))))
		(ListLink
			(GetLink (TypedVariable (Variable "$X") (Type 'ConceptNode))
				(Inheritance (Variable "$X") (Concept "Father")))
			(GetLink (TypedVariable (Variable "$X") (Type 'ConceptNode))
				(Inheritance (Variable "$X") (Concept "XeZir"))))))
