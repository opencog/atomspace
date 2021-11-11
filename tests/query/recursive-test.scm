;
; recursive-test.scm -- unit test for the recursive.scm demo.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "recursive-test")
(test-begin tname)

; ----------
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
(test-assert "mamvet" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(Present (Inheritance (Variable "this") (Variable "that")))
			(List	(Concept "mammal") (Concept "vertebrate"))))))

(test-assert "foovet" (equal? (stv 0 1)
	(cog-evaluate!
		(Evaluation
			(Present (Inheritance (Variable "this") (Variable "that")))
			(List	(Concept "foobar") (Concept "vertebrate"))))))

; ----------
(Define
	(DefinedPredicate "simple is-a relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(Present (Inheritance (Variable "this") (Variable "that")))))

(test-assert "mam-simp" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "simple is-a relation")
			(List	(Concept "mammal") (Concept "vertebrate"))))))

(test-assert "mam-lamb" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Present (Inheritance (Variable "this") (Variable "that"))))
			(List	(Concept "mammal") (Concept "vertebrate"))))))

; ----------
(test-assert "foo-abs" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(Absent (Inheritance (Variable "this") (Variable "that")))
			(List	(Concept "foobar") (Concept "vertebrate"))))))

(test-assert "mam-not-abs" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(Not (Absent (Inheritance (Variable "this") (Variable "that"))))
			(List	(Concept "mammal") (Concept "vertebrate"))))))

; ----------
(test-assert "hum-vert" (equal? (stv 1 1)
	(cog-evaluate!
		(Satisfaction
			(Present
				(Inheritance (Concept "human") (Variable "middle"))
				(Inheritance (Variable "middle") (Concept "vertebrate")))))))

(Define
	(DefinedPredicate "grandparent relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(Satisfaction
			(Variable "middle")
			(Present
				(Inheritance (Variable "this") (Variable "middle"))
				(Inheritance (Variable "middle") (Variable "that"))))))

(test-assert "foo-grand" (equal? (stv 0 1)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "grandparent relation")
			(List	(Concept "foobar") (Concept "vertebrate"))))))

(test-assert "foo-grand" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "grandparent relation")
			(List	(Concept "human") (Concept "vertebrate"))))))

; ----------
; First time, with an ordinary PutLink
(Define
	(DefinedPredicate "recursive relation")                 ;; Step 1.
	(Lambda
		(VariableList (Variable "this") (Variable "that"))   ;; Step 2.
		(SequentialOr                                        ;; Step 3.
			(Present
				(Inheritance (Variable "this") (Variable "that"))) ;; Step 4.
			(Satisfaction
				(Variable "middle")                            ;; Step 5.
				(And
					(Present                                    ;; Step 6.
						(Inheritance (Variable "this") (Variable "middle")))
					(Put                                        ;; Step 7.
						(DefinedPredicate "recursive relation")
						(List (Variable "middle") (Variable "that"))))))))

(test-assert "ben-anim" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "recursive relation")
			(List	(Concept "Ben") (Concept "animal"))))))

(test-assert "ben-foo" (equal? (stv 0 1)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "recursive relation")
			(List	(Concept "Ben") (Concept "foobar"))))))

; There are nine levels in the ontology above.
(test-assert "ben-list" (equal? 9 (length (cog-value->list
	(cog-execute!
		(Meet (TypedVariable (Variable "?inh") (Type 'Concept))
			(Put
				(DefinedPredicate "recursive relation")
				(List (Concept "Ben") (Variable "?inh")))))))))

; ----------
; Again with a continuation.
(Define
	(DefinedPredicate "inf regress")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(SequentialOr
			(Present
				(Inheritance (Variable "this") (Variable "that")))
			(Satisfaction
				(Variable "middle")
				(And
					(Present
						(Inheritance (Variable "this") (Variable "middle")))
					(Continuation
						(Put
							(DefinedPredicate "inf regress")
							(List (Variable "middle") (Variable "that")))))))))

(test-assert "cont-ben-anim" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "inf regress")
			(List	(Concept "Ben") (Concept "animal"))))))

(test-assert "cont-ben-foo" (equal? (stv 0 1)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "inf regress")
			(List	(Concept "Ben") (Concept "foobar"))))))

; There are nine levels in the ontology above.
(test-assert "cont-ben-list" (equal? 9 (length (cog-value->list
	(cog-execute!
		(Meet (TypedVariable (Variable "?inh") (Type 'Concept))
			(Put
				(DefinedPredicate "inf regress")
				(List (Concept "Ben") (Variable "?inh")))))))))

; ----------
; The infinte loop case.
(Inheritance (Concept "thing") (Concept "Ben"))

(test-assert "cont-finite-ben-anim" (equal? (stv 1 1)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "inf regress")
			(List	(Concept "Ben") (Concept "animal"))))))

(define (bonkers)
	(cog-evaluate!
		(Evaluation
			(DefinedPredicate "inf regress")
			(List	(Concept "Ben") (Concept "foobar")))))

(define throwd #f)
(catch #t bonkers (lambda (key args rest) (set! throwd #t)))
(test-assert "cont-inf-loop" throwd)

; ----------
(test-assert "exout" (equal? (stv 1 1)
	(cog-execute!
		(ExecutionOutput
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Present (Inheritance (Variable "this") (Variable "that"))))
			(List	(Concept "mammal") (Concept "vertebrate"))))))

(test-end tname)
