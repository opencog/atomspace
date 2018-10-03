((@@ (opencog) opencog-test-runner))

(define t "opencog-test-runner-pass")

; Test that exit value is propageted correctly on pass.
(test-begin t)

(test-assert "Passing-test" #t)

(test-end t)
