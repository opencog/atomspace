
(use-modules (opencog) (opencog test-runner))
(use-modules (opencog python))

(opencog-test-runner)
(define t "opencog-python-sniff-test")

(test-begin t)

; All that we do here is to make sure that python doesn't crash.
; `python-eval` returns whatever python returned, as a string.
(define rc (python-eval "print ('Hello world\\n', 2+2)"))

; Python print returns 'None'
(test-assert "python-eval is borken" (string=? rc "None"))

(test-end t)
