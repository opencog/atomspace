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

(define b17 (BoolValue 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1))
(define p17 (format #f "~A" b17))
(format #t "p17 = >>~A<<\n" p17)
(test-assert "p17"
	(equal? p17 "(BoolValue 0x1ee63)\n"))

(define b21 (BoolValue 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1))
(define p21 (format #f "~A" b21))
(format #t "p21 = >>~A<<\n" p21)
(test-assert "p21"
	(equal? p21 "(BoolValue 0x01ee63)\n"))

(test-end tname)

(opencog-test-end)
