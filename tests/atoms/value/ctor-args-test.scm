;
; ctor-args-test.scm -- Test that scheme Value constructors work properly
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "empty-ctor-test")
(test-begin tname)

(define nullb (BoolValue))
(format #t "Empty bool is ~A\n" nullb)
(test-assert "nullb" (equal? '() (cog-value->list nullb)))

(define nullf (FloatValue))
(format #t "Empty float is ~A\n" nullf)
(test-assert "nullf" (equal? '() (cog-value->list nullf)))

(define nullt (LinkValue))
(format #t "Empty list is ~A\n" nullt)
(test-assert "nullt" (equal? '() (cog-value->list nullt)))

(define nulls (StringValue))
(format #t "Empty string is ~A\n" nulls)
(test-assert "nulls" (equal? '() (cog-value->list nulls)))

(test-end tname)

(opencog-test-end)
