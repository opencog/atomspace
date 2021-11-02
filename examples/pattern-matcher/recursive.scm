;
; recursive.scm -- A recursive chain of queries.
;
; This demo illustrates how recursive queries can be written. It shows
; a very simple kind of forward chaining, made possible by sequencing
; primitive operations together in sequential execution.
;
(use-modules (opencog) (opencog exec))

; Populate the AtomSpace with a tiny fragment of an upper ontology.
; The demo will be chaining these together, to reach conclusions
; about relationships.
(Inheritance (Concept "physical thing") (Concept "thing"))
(Inheritance (Concept "living thing") (Concept "physical thing"))
(Inheritance (Concept "animal") (Concept "living thing"))
(Inheritance (Concept "bilateria") (Concept "animal"))
(Inheritance (Concept "chordate") (Concept "bilateria"))
(Inheritance (Concept "vertebrate") (Concept "chordate"))
(Inheritance (Concept "mammal") (Concept "vertebrate"))
(Inheritance (Concept "human") (Concept "mammal"))
(Inheritance (Concept "Ben") (Concept "human"))

; Define a very simple is-a relationship. It defines a predicate that
; takes two arguments: "this" and "that", and looks to see if the two
; inherit from each other. When evaluated, it will return true if the
; AtomSpace contains an InheritanceLink connecting "this" and "that".
; It explicitly checks for the presence of such a link, with the
; PresentLink.
(Define
	(DefinedPredicate "simple is-a relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(Present (Inheritance (Variable "this") (Variable "that")))))

; Lets check if mammals are vertebrates. This should return the true
; TV, i.e. (SimpleTruthValue 1 1)  aka (stv 1 1)
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "simple is-a relation")
		(List	(Concept "mammal") (Concept "vertebrate"))))

; The same query also works without the intervening Define; one can
; stick the Lambda directly into place in the EvaluationLink.
(cog-evaluate!
	(Evaluation
		(Lambda
			(VariableList (Variable "this") (Variable "that"))
			(Present (Inheritance (Variable "this") (Variable "that"))))
		(List	(Concept "mammal") (Concept "vertebrate"))))

; We can verify that nonsense returns false aka (stv 0 1).
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "simple is-a relation")
		(List	(Concept "foobar") (Concept "vertebrate"))))

; There is an explicit AbsentLink, as well. It's the opposite of the
; PresentLink.
(cog-evaluate!
	(Evaluation
		(Lambda
			(VariableList (Variable "this") (Variable "that"))
			(Absent (Inheritance (Variable "this") (Variable "that"))))
		(List	(Concept "foobar") (Concept "vertebrate"))))

; Of course, we could have said "not present"; the AbsentLink is not
; really needed for this demo; it is far more useful and powerful
; when it appears in patterns.
(cog-evaluate!
	(Evaluation
		(Lambda
			(VariableList (Variable "this") (Variable "that"))
			(Not (Absent (Inheritance (Variable "this") (Variable "that")))))
		(List	(Concept "mammal") (Concept "vertebrate"))))


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

