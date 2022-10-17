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

; ------------------------------------------------------
; Do it again, one level up.
(define top-space (cog-new-atomspace base-space))
(cog-set-atomspace! top-space)

; x1 is still in the base.
(define x1 (Concept "foo"))
(define x2 (cog-set-value! x1 (Predicate "gee") (ctv 1 0 4)))
(test-equal "base-x1" base-space (cog-atomspace x1))
(test-equal "top-x2" top-space (cog-atomspace x2))
(test-equal 1 (get-val x1 "gee"))
(test-equal 4 (get-val x2 "gee"))
(test-equal 4 (get-val (Concept "foo") "gee"))

(define y1 (Concept "bar"))
(define y2 (cog-set-value! y1 (Predicate "gosh") (ctv 1 0 5)))
(test-equal "top-y1" base-space (cog-atomspace y1))
(test-equal "top-y2" top-space (cog-atomspace y2))
(test-equal 2 (get-val y1 "gosh"))
(test-equal 5 (get-val y2 "gosh"))
(test-equal 5 (get-val (Concept "bar") "gosh"))

(define z1 (List x1 y1))
(define z2 (List x2 y2))
(define z2v (cog-set-value! z1 (Predicate "bang") (ctv 1 0 6)))
(test-equal "top-z1" base-space (cog-atomspace z1))
(test-equal "top-z2" top-space (cog-atomspace z2))
(test-equal "top-z2v" top-space (cog-atomspace z2v))
(test-equal 3 (get-val z1 "bang"))
(test-equal 6 (get-val z2 "bang"))
(test-equal 6 (get-val (List x1 y1) "bang"))
(test-equal 6 (get-val (List x2 y2) "bang"))
(test-equal 6 (get-val (List x y) "bang"))
(test-equal 6 (get-val (List (Concept "foo") (Concept "bar")) "bang"))

; ------------------------------------------------------
; Go back to the base space, check that its valid
(cog-set-atomspace! base-space)

(define xb (Concept "foo"))
(test-equal "base-xb" base-space (cog-atomspace xb))
(test-equal 1 (get-val xb "gee"))
(test-equal 1 (get-val (Concept "foo") "gee"))

(define yb (Concept "bar"))
(test-equal "base-yb" base-space (cog-atomspace yb))
(test-equal 2 (get-val yb "gosh"))
(test-equal 2 (get-val (Concept "bar") "gosh"))

(define zb (List xb yb))
(test-equal "base-zb" base-space (cog-atomspace zb))
(test-equal 3 (get-val zb "bang"))
(test-equal 3 (get-val (List xb yb) "bang"))
(test-equal 3 (get-val (List (Concept "foo") (Concept "bar")) "bang"))


(test-end basic-link)

; -------------------------------------------------------------------
(opencog-test-end)
