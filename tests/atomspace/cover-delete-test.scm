;
; cover-space-test.scm
; Verify that links are correctly covered/uncovered in deeply nested
; atomspaces.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(opencog-test-runner)

; -------------------------------------------------------------------
; Common setup, used by all tests.

(define base-space (cog-atomspace))
(cog-atomspace-cow! #t base-space)

(define mid1-space (cog-new-atomspace base-space))
(cog-atomspace-cow! #t mid1-space)

(define mid2-space (cog-new-atomspace mid1-space))
(cog-atomspace-cow! #t mid2-space)

(define mid3-space (cog-new-atomspace mid2-space))
(cog-atomspace-cow! #t mid3-space)

(define surface-space (cog-new-atomspace mid3-space))
(cog-atomspace-cow! #t surface-space)

; ===================================================================
; Test that deep links are found correctly.

(define deep-delete "test deep delete")
(test-begin deep-delete)

; Repeatedly add and remove the same atom
(cog-set-atomspace! base-space)
(Concept "foo" (ctv 1 0 3))

(cog-set-atomspace! mid1-space)
(cog-extract! (Concept "foo"))

(cog-set-atomspace! mid2-space)
(Concept "foo" (ctv 1 0 5))

(cog-set-atomspace! mid3-space)
(cog-extract! (Concept "foo"))

(cog-set-atomspace! surface-space)
(Concept "foo" (ctv 1 0 7))

; -----------------------------------
; Should be present in the base space.
(cog-set-atomspace! base-space)
(test-assert "base-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "base-tv" 3 (inexact->exact (cog-count (cog-node 'Concept "foo"))))

; Should be absent in the next level.
(cog-set-atomspace! mid1-space)
(test-assert "mid1-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! mid2-space)
(test-assert "mid2-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "mid2-tv" 5 (inexact->exact (cog-count (cog-node 'Concept "foo"))))

(cog-set-atomspace! mid3-space)
(test-assert "mid3-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! surface-space)
(test-assert "surface-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "surface-tv" 7 (inexact->exact (cog-count (cog-node 'Concept "foo"))))

(test-end deep-delete)

; ===================================================================
; Building on the above, verify that values work

(define deep-change "test deep change-delete")
(test-begin deep-change)

; Repeatedly add and remove the same atom
(cog-set-atomspace! base-space)
(cog-set-tv! (Concept "foo") (ctv 1 0 2))

(cog-set-atomspace! mid2-space)
(cog-set-tv! (Concept "foo") (ctv 1 0 4))

(cog-set-atomspace! surface-space)
(cog-set-tv! (Concept "foo") (ctv 1 0 6))

; -----------------------------------
; Should be present in the base space.
(cog-set-atomspace! base-space)
(test-assert "base-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "base-tv" 2 (inexact->exact (cog-count (cog-node 'Concept "foo"))))

; Should be absent in the next level.
(cog-set-atomspace! mid1-space)
(test-assert "mid1-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! mid2-space)
(test-assert "mid2-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "mid2-tv" 4 (inexact->exact (cog-count (cog-node 'Concept "foo"))))

(cog-set-atomspace! mid3-space)
(test-assert "mid3-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! surface-space)
(test-assert "surface-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "surface-tv" 6 (inexact->exact (cog-count (cog-node 'Concept "foo"))))

(test-end deep-change)

; ===================================================================
(opencog-test-end)
