;
; recursive.scm -- A recursive chain of queries.
;
; This demo illustrates how recursive queries can be written. It shows
; a very simple kind of forward chaining, made possible by sequencing
; primitive operations together in sequential execution.
;
(use-modules (opencog) (opencog exec))

; ----------
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

; ----------
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

; ----------
; Here's what we cannot do, or rather, should not do: we should not
; directly assert that
;    (Present (Inheritance  (Concept "foo") (Concept "vertebrate")))
; because doing so would have the side-effect of inserting the
; relationship between "foo" and vertbrates directly into the AtomSpace.
; This is why the above structures are fairly complex: they are using
; variables to avoid accidentally asserting new relationships into the
; AtomSpace. When the cog-evaluate! runs, it of course subsitutes
; (beta-reduces) the Concepts into the Variables, and then evaluates
; the result. But the evaluation is done in such a manner that the
; AtomSpace is not polluted with InheritanceLinks between things that
; don't belong.
;
; For example, below, we check for nonsense, for a "foobar" relation.
; That comes out false, and the contents of the AtomSpace are not
; corrupted in the process.
; ----------

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

; ----------
; Verifying a grand-parent/grand-child relationship requires moving to a
; different syntax. The query below requests that something in the
; middle is present in the AtomSpace.
(cog-evaluate!
	(Satisfaction
		(Present
			(Inheritance (Concept "human") (Variable "middle"))
			(Inheritance (Variable "middle") (Concept "vertebrate")))))

; The above works fine, and is resonably readable. The only problem
; is that the two constants appear in the middle of the expression.
; These can be pulled out with a Lambda. It is a bit verbose, but
; it allows a generic predicate to be defined.
(Define
	(DefinedPredicate "grandparent relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(Satisfaction
			(Variable "middle")
			(Present
				(Inheritance (Variable "this") (Variable "middle"))
				(Inheritance (Variable "middle") (Variable "that"))))))

; Thise new predicate can be used exactly the same way as the earlier
; one.  All the intervening details have been hidden.
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "grandparent relation")
		(List	(Concept "foobar") (Concept "vertebrate"))))

; We can check that it indeed "skips a step" correctly.
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "grandparent relation")
		(List	(Concept "human") (Concept "vertebrate"))))

; ----------
; What about the general recursive case? This is sadly rathr verbose,
; but here it is. To unpack it verbally:
; 1) Start with a definition: the name of the recursive function.
; 2) The lambda: it binds two variables, as before.
; 3) A SequentialOr. Evaluation stops as soon as one of the terms
;    returns true.
; 4) A direct check of inheritance. If this evaluates to true, then
;    we are done.
; 5) If not, then a SatisfactionLink, much as before.
; 6) The SatisfactionLink is looking to see if "this" is connected
;    to the middle.
; 7) But we also need the middle connected to a recursively long
;    chain. To get that, we refer to the recursive definition itself.
;    That defintion takes two arguments. But which two arguments?
;    The PutLink explains exactly which two: the middle, and the
;    other endpoint.
;
(Define
	(DefinedPredicate "recursive relation")                 ;; Step 1)
	(Lambda
		(VariableList (Variable "this") (Variable "that"))   ;; Step 2)
		(SequentialOr                                        ;; Step 3)
			(Present
				(Inheritance (Variable "this") (Variable "that"))) ;; Step 4)
			(Satisfaction
				(Variable "middle")                            ;; Step 5)
				(And
					(Present                                    ;; Step 6)
						(Inheritance (Variable "this") (Variable "middle")))
					(Put                                        ;; Step 7)
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

