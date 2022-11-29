;
; proxy-node-test.scm -- Unit test for basic ProxyNode syntax.
;
(use-modules (opencog) (opencog persist))
(use-modules (opencog exec))
(use-modules (opencog test-runner))

; ---------------------------------------------------------------------
(opencog-test-runner)
(define tname "proxy-node")
(test-begin tname)

; This should lead to failure.
(define foo (WriteThruProxy "foo"))
(ProxyParametersLink foo (Concept "bar"))

(define caught #f)
(catch 'C++-EXCEPTION (lambda () (cog-open foo))
	(lambda (key funcname msg)
		(format #t "Yes, we got the eception as expected\n")
		(set! caught #t)))

(test-assert "Failed to catch exception" caught)

(test-end tname)

(opencog-test-end)
