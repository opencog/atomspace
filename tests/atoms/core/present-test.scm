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

; Run the EvaluatiionLink, lambda style
(define (run-lamb-eval type str)
	(cog-evaluate!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(type (Inheritance (Variable "this") (Variable "that"))))
			(List (Concept str) (Concept "chordate")))))

(define (run-lnot-eval type str)
	(cog-evaluate!
		(Evaluation
			(Lambda
				(VariableList (Variable "this") (Variable "that"))
				(Not (type (Inheritance (Variable "this") (Variable "that")))))
			(List (Concept str) (Concept "chordate")))))

(test-assert "vert" (equal? (stv 1 1) (run-lamb-eval Present "vertebrate")))
(test-assert "junk" (equal? (stv 0 1) (run-lamb-eval Present "asdfasdf")))
(test-assert "jabs" (equal? (stv 1 1) (run-lamb-eval Absent "asdfasdf")))
(test-assert "jnot" (equal? (stv 0 1) (run-lnot-eval Absent "asdfasdf")))
(test-assert "vnot" (equal? (stv 1 1) (run-lnot-eval Absent "vertebrate")))

; Same as above, bare style
(define (run-bare-eval type str)
	(cog-evaluate!
		(Evaluation
			(type (Inheritance (Variable "this") (Variable "that")))
			(List (Concept str) (Concept "chordate")))))

(define (run-bnot-eval type str)
	(cog-evaluate!
		(Evaluation
			(Not (type (Inheritance (Variable "this") (Variable "that"))))
			(List (Concept str) (Concept "chordate")))))

(test-assert "bvert" (equal? (stv 1 1) (run-bare-eval Present "vertebrate")))
(test-assert "bjunk" (equal? (stv 0 1) (run-bare-eval Present "asdfasdf")))
(test-assert "bjabs" (equal? (stv 1 1) (run-bare-eval Absent "asdfasdf")))
(test-assert "bjnot" (equal? (stv 0 1) (run-bnot-eval Absent "asdfasdf")))
(test-assert "bvnot" (equal? (stv 1 1) (run-bnot-eval Absent "vertebrate")))

(test-end tname)
