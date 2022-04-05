(use-modules (opencog)
             (opencog test-runner))

(opencog-test-runner)
(define t "opencog-test-runner-pass")

; Test that exit value is propageted correctly on pass.
(test-begin t)

; The test could be written such that the atomspace isn't used, but since
; this test acts as an example for how to use the test-runner with cmake
; configuration, doing  so will prevent showing how tests should be
; configured when the scheme code is a wrapper to a c++ library.
(test-equal "Passing-test" (Node "a") (Node "a"))

(test-end t)

(opencog-test-end)
