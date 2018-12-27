(use-modules (opencog))
(use-modules (opencog test-runner))
(use-modules (opencog python))

(opencog-test-runner)
(define t "opencog-python-sniff-test")

(test-begin t)

; Ideally, this will not crash.  Currently, it returns an empty string.
(define rc (python-eval "print ('Hello world\\n', 2+2)"))

(test-assert "python-eval is borken" (string=? rc ""))

(test-end t)
