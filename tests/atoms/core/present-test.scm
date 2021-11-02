;
; present-test.scm -- test that PresentLink, AbsentLink work.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "present-test")
(test-begin tname)

; Some data
(Inheritance (Concept "chordate") (Concept "bilateria"))
(Inheritance (Concept "vertebrate") (Concept "chordate"))

(define tv-a
	(cog-evaluate!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Present (Inheritance (Variable "this") (Variable "that"))))
			(List (Concept "vertebrate") (Concept "chordate")))))

(test-assert "vtb is chr" (equal? tv-a (stv 1 1)))

(define tv-b
	(cog-evaluate!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Present (Inheritance (Variable "this") (Variable "that"))))
			(List (Concept "asdfasdf") (Concept "chordate")))))

(test-assert "junk is chr" (equal? tv-b (stv 0 1)))

(define tv-c
	(cog-evaluate!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Absent (Inheritance (Variable "this") (Variable "that"))))
			(List (Concept "asdfasdf") (Concept "chordate")))))

(test-assert "junk is not chr" (equal? tv-c (stv 1 1)))

(define tv-d
	(cog-evaluate!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Not (Absent (Inheritance (Variable "this") (Variable "that")))))
			(List (Concept "asdfasdf") (Concept "chordate")))))

(test-assert "not junk is not chr" (equal? tv-d (stv 0 1)))

(define tv-e
	(cog-evaluate!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Not (Absent (Inheritance (Variable "this") (Variable "that")))))
			(List (Concept "vertebrate") (Concept "chordate")))))

(test-assert "not vtb is not chr" (equal? tv-e (stv 1 1)))

(test-end tname)
