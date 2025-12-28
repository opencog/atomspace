#! /usr/bin/env guile
-s
!#
;
; ctor-args-test.scm -- Test that scheme Value constructors work properly
;
(use-modules (opencog))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "empty-ctor-test")
(test-begin tname)

;; Zero-length vectors are allowed.
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

(define tname "list-ctor-test")
(test-begin tname)

;; list vectors are allowed.
(define listf (FloatValue (list 1 2 3 4)))
(format #t "List float is ~A\n" listf)
(test-assert "list" (equal? 4 (length (cog-value->list listf))))

(test-end tname)

(opencog-test-end)
