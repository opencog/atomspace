((@@ (opencog) opencog-test-runner))

(define t "opencog-test-runner-fail")

; Test that exit value is propageted correctly on fail.
(test-begin t)

(test-assert "Failing-test" #f)

(test-end t)
