;
; bool-value-print-test.scm -- Verify that BoolValue to_string() works
; (It doesn't; it needs a length value, but no one cares right now)
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "incoming-of-test")
(test-begin tname)

(define b15 (BoolValue 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0))
(define p15 (format #f "~A" b15))
(format #t "p15 = >>~A<<\n" p15)
(test-assert "p15"
	(equal? p15 "(BoolValue 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0)\n"))

(define b16 (BoolValue 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1))
(define p16 (format #f "~A" b16))
(format #t "p16 = >>~A<<\n" p16)
(test-assert "p16"
	(equal? p16 "(BoolValue 0xf731)\n"))

(test-end tname)

(opencog-test-end)
