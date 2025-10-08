
(use-modules (opencog) (opencog test-runner))
(use-modules (opencog python))

(opencog-test-runner)
(define t "opencog-python-sniff-test")

(test-begin t)

; All that we do here is to make sure that python doesn't crash.
; `python-eval` returns whatever python returned, as a string.
(define rc (python-eval "print ('Hello world\\n', 2+2)"))

; Python print itself evaluates to 'None'. But before that, it
; prints stuff to stdout, which we expect to see.
(define expected "Hello world\n 4\n")

; CxxTest steals away stdout, and eats the result of the print
; statement. So if we are running this test in the CxxTest harness
; then there's no "Hello world" (but if you run it by hand, there is.)
; (define expected-in-cxxtest "None\n")  ; None is no longer returned!
(define expected-in-cxxtest "")

(format #t "python-eval returned this: >>~A<<\n" rc)

(test-assert "Oh no! python-eval is borken!"
	(or (string=? rc expected)
		(string=? rc expected-in-cxxtest)))

(test-end t)

(opencog-test-end)
