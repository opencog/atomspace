;
; ctor-args-test.scm -- Test that scheme Value constructors work properly
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "ctor-args-test")
(test-begin tname)

(define nullb (BoolValue))
(format #t "Empty bool is ~A\n" nullb)
(test-assert "nullb" (equal? () (cog-value->list nullb)))

(test-end tname)

(opencog-test-end)
