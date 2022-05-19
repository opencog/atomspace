;
; put-recursive-test.scm -- test for crash on recursive PutLink
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "put-recursive-test")
(test-begin tname)

; Define a recursive Schema
(Define
	(DefinedSchema "is-a relation")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(SequentialOr
			(Inheritance (Variable "this") (Variable "that"))
			(SequentialAnd
				(Inheritance (Variable "this") (Variable "middle"))
				(Put (DefinedSchema "is-a relation")
					(List (Variable "middle") (Variable "that")))))))

; Attempt to use it
(define is-it
	(Put
		(DefinedSchema "is-a relation")
		(List (Concept "human") (Concept "chordate"))))

(cog-execute! is-it)

; If we reached this part without crashing, we're good.
(test-assert "no crash" #t)

(test-end tname)

(opencog-test-end)
