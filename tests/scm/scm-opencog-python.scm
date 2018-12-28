
; For reasons I cannot comprehend, circle-ci fails with an error message:
;
;;; compiling /ws/atomspace/tests/scm/scm-opencog-python.scm
;;; WARNING: compilation of /ws/atomspace/tests/scm/scm-opencog-python.scm failed:
;;; no code for module (opencog)
;
; I don't know why it fails. But it does.
(use-modules (opencog))
; (load-from-path "opencog.scm")

(use-modules (opencog test-runner))
(use-modules (opencog python))

(opencog-test-runner)
(define t "opencog-python-sniff-test")

(test-begin t)

; Ideally, this will not crash.  Currently, it returns an empty string.
(define rc (python-eval "print ('Hello world\\n', 2+2)"))

(test-assert "python-eval is borken" (string=? rc ""))

(test-end t)
