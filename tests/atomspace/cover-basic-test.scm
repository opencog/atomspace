;
; cover-basic-test.scm
; Verify basic operations of layere atomspace frames.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(opencog-test-runner)

(define (get-val ATOM NAME) (inexact->exact
   (cog-value-ref (cog-value ATOM (Predicate NAME)) 2)))

; -------------------------------------------------------------------
; Verify basic value-setting.

(define basic-link "test basic links")
(test-begin basic-link)

(define base-space (cog-atomspace))

; Create some basic atoms.
(define x (Concept "foo"))
(cog-set-value! x (Predicate "gee") (ctv 1 0 1))
(test-equal "base-x" base-space (cog-atomspace x))
(test-equal 1 (get-val x "gee"))
(test-equal 1 (get-val (Concept "foo") "gee"))

(define y (Concept "bar"))
(cog-set-value! y (Predicate "gosh") (ctv 1 0 2))
(test-equal "base-y" base-space (cog-atomspace y))
(test-equal 2 (get-val y "gosh"))
(test-equal 2 (get-val (Concept "bar") "gosh"))

(define z (List x y))
(cog-set-value! z (Predicate "bang") (ctv 1 0 3))
(test-equal "base-z" base-space (cog-atomspace z))
(test-equal 3 (get-val z "bang"))
(test-equal 3 (get-val (List x y) "bang"))
(test-equal 3 (get-val (List (Concept "foo") (Concept "bar")) "bang"))


(test-end basic-link)

; -------------------------------------------------------------------
(opencog-test-end)
