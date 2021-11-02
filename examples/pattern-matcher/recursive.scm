;
; recursive.scm
;

(use-modules (opencog) (opencog exec))

(Inheritance (Concept "physical thing") (Concept "thing"))
(Inheritance (Concept "living thing") (Concept "physical thing"))
(Inheritance (Concept "animal") (Concept "living thing"))
(Inheritance (Concept "bilateria") (Concept "animal"))
(Inheritance (Concept "chordate") (Concept "bilateria"))
(Inheritance (Concept "vertebrate") (Concept "chordate"))
(Inheritance (Concept "mammal") (Concept "vertebrate"))
(Inheritance (Concept "human") (Concept "mammal"))
(Inheritance (Concept "Ben") (Concept "human"))

(Define
	(DefinedPredicate "simple is-a relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(Present (Inheritance (Variable "this") (Variable "that")))))

(define simple-is-a
	(Evaluation
		(DefinedPredicate "simple is-a relation")
		(List	(Concept "mammal") (Concept "vertebrate"))))

(cog-evaluate! simple-is-a)

(cog-evaluate!
	(Evaluation
		(Lambda
			(VariableList (Variable "this") (Variable "that"))
			(Present (Inheritance (Variable "this") (Variable "that"))))
		(List	(Concept "mammal") (Concept "vertebrate"))))


(cog-evaluate!
	(Evaluation
		(DefinedPredicate "simple is-a relation")
		(List	(Concept "foobar") (Concept "vertebrate"))))

(Define
	(DefinedPredicate "grandparent relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(Satisfaction
			(Variable "middle")
			(Present
				(Inheritance (Variable "this") (Variable "middle"))
				(Inheritance (Variable "middle") (Variable "that"))))))

(cog-evaluate!
	(Evaluation
		(DefinedPredicate "grandparent relation")
		(List	(Concept "foobar") (Concept "vertebrate"))))

(cog-evaluate!
	(Evaluation
		(DefinedPredicate "grandparent relation")
		(List	(Concept "human") (Concept "vertebrate"))))

(Define
	(DefinedPredicate "recursive relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(SequentialOr
			(Present (Inheritance (Variable "this") (Variable "that")))
			(Satisfaction
				(Variable "middle")
				(And
					(Present
						(Inheritance (Variable "this") (Variable "middle")))
					(Put
						(DefinedPredicate "recursive relation")
						(List (Variable "middle") (Variable "that"))))))))

(cog-evaluate!
	(Evaluation
		(DefinedPredicate "recursive relation")
		(List	(Concept "Ben") (Concept "animal"))))



(cog-execute!
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "this") (Variable "that"))
			(Present (Inheritance (Variable "this") (Variable "that"))))
		(List	(Concept "mammal") (Concept "vertebrate"))))

