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

; Run the EvaluationLink, lambda style
(define (run-lamb-eval type str)
	(cog-execute!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(type (Inheritance (Variable "this") (Variable "that"))))
			(List (Concept str) (Concept "chordate")))))

(define (run-lnot-eval type str)
	(cog-execute!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Not (type (Inheritance (Variable "this") (Variable "that")))))
			(List (Concept str) (Concept "chordate")))))

(test-assert "vert" (equal? (BoolValue #t) (run-lamb-eval Present "vertebrate")))
(test-assert "junk" (equal? (BoolValue #f) (run-lamb-eval Present "asdfasdf")))
(test-assert "jabs" (equal? (BoolValue #t) (run-lamb-eval Absent "asdfasdf")))
(test-assert "jnot" (equal? (BoolValue #f) (run-lnot-eval Absent "asdfasdf")))
(test-assert "vnot" (equal? (BoolValue #t) (run-lnot-eval Absent "vertebrate")))

; Same as above, bare style
(define (run-bare-eval type str)
	(cog-execute!
		(Evaluation
			(type (Inheritance (Variable "this") (Variable "that")))
			(List (Concept str) (Concept "chordate")))))

(define (run-bnot-eval type str)
	(cog-execute!
		(Evaluation
			(Not (type (Inheritance (Variable "this") (Variable "that"))))
			(List (Concept str) (Concept "chordate")))))

(test-assert "bvert" (equal? (BoolValue #t) (run-bare-eval Present "vertebrate")))
(test-assert "bjunk" (equal? (BoolValue #f) (run-bare-eval Present "asdfasdf")))
(test-assert "bjabs" (equal? (BoolValue #t) (run-bare-eval Absent "asdfasdf")))
(test-assert "bjnot" (equal? (BoolValue #f) (run-bnot-eval Absent "asdfasdf")))
(test-assert "bvnot" (equal? (BoolValue #t) (run-bnot-eval Absent "vertebrate")))

(test-end tname)

(opencog-test-end)
