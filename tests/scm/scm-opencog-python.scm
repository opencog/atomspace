
(use-modules (opencog) (opencog test-runner))
(use-modules (opencog python))

(opencog-test-runner)
(define t "opencog-python-sniff-test")

(test-begin t)

; All tht we do here is to make sure that python doesn't crash.
; Currently, `python-eval` returns an empty string.
(define rc (python-eval "print ('Hello world\\n', 2+2)"))

(test-assert "python-eval is borken" (string=? rc ""))

(test-end t)
