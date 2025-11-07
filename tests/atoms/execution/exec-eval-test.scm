;
; exec-eval-test.scm -- Verify that ExecutionOutput can run
; evaluatable links.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

;;; (use-modules (opencog logger))
;;; (cog-logger-set-stdout! #t)
;;; (cog-logger-set-level! "FINE")

(opencog-test-runner)
(define tname "exec-eval-test")
(test-begin tname)

(define exec-plus
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "A") (Variable "B"))
			(Plus (Variable "A") (Variable "B")))
		(List (Number 2) (Number 3))))

(test-assert "Exec Plus"
	(equal? (cog-execute! exec-plus) (Number 5)))

; ------------------------------------

(define exec-vec
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "A") (Variable "B"))
			(Heaviside (Minus (Variable "A") (Variable "B"))))
		(List (Number 2 3 4 5) (Number 3 2 1 8))))

(test-assert "Exec Plus"
	(equal? (cog-execute! exec-vec) (Number 0 1 1 0)))

; ------------------------------------

(define exec-less-true
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "A") (Variable "B"))
			(LessThan (Variable "A") (Variable "B")))
		(List (Number 2) (Number 3))))

(test-assert "Exec Less True"
	(equal? (cog-execute! exec-less-true) (BoolValue #t)))

; ------------------------------------

(define exec-less-false
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "A") (Variable "B"))
			(LessThan (Variable "A") (Variable "B")))
		(List (Number 3) (Number 2))))

(test-assert "Exec Less False"
	(equal? (cog-execute! exec-less-false) (BoolValue #f)))

(test-end tname)

(opencog-test-end)
