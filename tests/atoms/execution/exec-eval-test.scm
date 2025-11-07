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

; ------------------------------------

(define exec-plus
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "A") (Variable "B"))
			(Plus (Variable "A") (Variable "B")))
		(List (Number 2) (Number 3))))

(define exec-plus-size (count-all))
(test-assert "Exec Plus"
	(equal? (cog-execute! exec-plus) (Number 5)))

; The AtomSpace should not have been polluted with scratch results!
; (plus one for the funal result)
(test-assert "Exec Plus Size"
	(equal? (+ exec-plus-size 1) (count-all)))

; ------------------------------------

(define exec-plus-bare
	(ExecutionOutput
		(Plus)
		(List (Number 39) (Number 3))))

(define exec-plus-bare-size (count-all))
(test-assert "Exec Plus Bare"
	(equal? (cog-execute! exec-plus-bare) (Number 42)))

; The AtomSpace should not have been polluted with scratch results!
; (plus one for the new number ...)
(test-assert "Exec Plus Size"
	(equal? (+ exec-plus-bare-size 1) (count-all)))

; ------------------------------------

(define exec-vec
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "A") (Variable "B"))
			(Heaviside (Minus (Variable "A") (Variable "B"))))
		(List (Number 2 3 4 5) (Number 3 2 1 8))))

(define exec-vec-size (count-all))
(test-assert "Exec Vec"
	(equal? (cog-execute! exec-vec) (Number 0 1 1 0)))

; The AtomSpace should not have been polluted with scratch results!
; (plus one for the new number ...)
(test-assert "Exec Vec Size"
	(equal? (+ exec-vec-size 1) (count-all)))

; ------------------------------------

(define exec-less-true
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "A") (Variable "B"))
			(LessThan (Variable "A") (Variable "B")))
		(List (Number 2) (Number 3))))

(define exec-less-true-size (count-all))
(test-assert "Exec Less True"
	(equal? (cog-execute! exec-less-true) (BoolValue #t)))

; The AtomSpace should not have been polluted with scratch results!
(test-assert "Exec Less True Size"
	(equal? exec-less-true-size (count-all)))

; ------------------------------------

(define exec-less-false
	(ExecutionOutput
		(Lambda
			(VariableList (Variable "A") (Variable "B"))
			(LessThan (Variable "A") (Variable "B")))
		(List (Number 3) (Number 2))))

(define exec-less-false-size (count-all))
(test-assert "Exec Less False"
	(equal? (cog-execute! exec-less-false) (BoolValue #f)))

; The AtomSpace should not have been polluted with scratch results!
(test-assert "Exec Less False Size"
	(equal? exec-less-false-size (count-all)))

; ------------------------------------

(define exec-less-bare-true
	(ExecutionOutput
		(LessThan)
		(List (Number 6) (Number 7))))

(define exec-less-bare-true-size (count-all))
(test-assert "Exec Less Bare True"
	(equal? (cog-execute! exec-less-bare-true) (BoolValue #t)))

; The AtomSpace should not have been polluted with scratch results!
(test-assert "Exec Less Bare True Size"
	(equal? exec-less-bare-true-size (count-all)))

; ------------------------------------

(define exec-less-bare-false
	(ExecutionOutput
		(LessThan)
		(List (Number 9) (Number 7))))

(define exec-less-bare-false-size (count-all))
(test-assert "Exec Less Bare False"
	(equal? (cog-execute! exec-less-bare-false) (BoolValue #f)))

; The AtomSpace should not have been polluted with scratch results!
(test-assert "Exec Less Bare False Size"
	(equal? exec-less-bare-false-size (count-all)))

; ------------------------------------

(test-end tname)
(opencog-test-end)
