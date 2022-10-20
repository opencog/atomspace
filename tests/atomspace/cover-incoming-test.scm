;
; cover-incoming-test.scm
; Verify that incoming sets are correctly computed in nested
; atomspaces.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(opencog-test-runner)

(define (get-cnt ATOM) (inexact->exact (cog-count ATOM)))

; -------------------------------------------------------------------
; Common setup, used by all tests.

(define base-space (cog-atomspace))
(define mid1-space (cog-new-atomspace base-space))
(define mid2-space (cog-new-atomspace mid1-space))
(define mid3-space (cog-new-atomspace mid2-space))
(define mid4-space (cog-new-atomspace mid3-space))
(define mid5-space (cog-new-atomspace mid4-space))
(define mid6-space (cog-new-atomspace mid5-space))
(define top-space (cog-new-atomspace mid6-space))

; Splatter some atoms into the various spaces.
(cog-set-atomspace! base-space)
(Concept "foo" (ctv 1 0 3))
(Concept "bar" (ctv 1 0 4))

(cog-set-atomspace! mid1-space)
(ListLink (Concept "foo") (Concept "bar") (ctv 1 0 5))

(cog-set-atomspace! mid2-space)
(cog-extract-recursive! (Concept "foo"))

(cog-set-atomspace! mid3-space)
(Concept "foo" (ctv 1 0 6))
(List (Concept "foo") (Concept "x"))
(Set (Concept "foo") (Concept "s"))

(cog-set-atomspace! mid4-space)
(ListLink (Concept "foo") (Concept "bar") (ctv 1 0 7))

(cog-set-atomspace! mid5-space)
(ListLink (Concept "foo") (Concept "bar") (ctv 1 0 8))

(cog-set-atomspace! mid6-space)
(Concept "foo" (ctv 1 0 9))

(cog-set-atomspace! top-space)

; -------------------------------------------------------------------
; Test that incoming sets in complex situations.

(define complex-inco "test complex incoming")
(test-begin complex-inco)

(define foo (Concept "foo"))
(test-equal "foo-inset-sz" 3 (cog-incoming-size foo))
(test-equal "foo-inset" 3 (length (cog-incoming-set foo)))

(test-equal "foo-lk-inset-sz" 2 (cog-incoming-size-by-type foo 'List))
(test-equal "foo-lk-inset" 2 (length (cog-incoming-by-type foo 'List)))

(test-equal "foo-st-inset-sz" 1 (cog-incoming-size-by-type foo 'Set))
(test-equal "foo-st-inset" 1 (length (cog-incoming-by-type foo 'Set)))

; Expect less, in the lower spaces.
(test-equal "mfoo-lk-inset-sz" 1 (cog-incoming-size-by-type foo 'List mid2-space))
(test-equal "mfoo-lk-inset" 1 (length (cog-incoming-by-type foo 'List mid2-space)))

(test-equal "mfoo-st-inset-sz" 0 (cog-incoming-size-by-type foo 'Set mid2-space))
(test-equal "mfoo-st-inset" 0 (length (cog-incoming-by-type foo 'Set mid2-space)))

; None at the bottom
(test-equal "mfoo-lk-inset-sz" 0 (cog-incoming-size-by-type foo 'List mid1-space))
(test-equal "mfoo-lk-inset" 0 (length (cog-incoming-by-type foo 'List mid1-space)))

(test-equal "mfoo-st-inset-sz" 0 (cog-incoming-size-by-type foo 'Set mid1-space))
(test-equal "mfoo-st-inset" 0 (length (cog-incoming-by-type foo 'Set mid1-space)))

(test-end complex-inco)

; -------------------------------------------------------------------
(opencog-test-end)
