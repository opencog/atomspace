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
(define mid1-space (cog-new-atomspace base-space))
(define mid2-space (cog-new-atomspace mid1-space))
(define surface-space (cog-new-atomspace mid2-space))

; -------------------------------------------------------------------
; Test that deep links are found correctly.

(define deep-link "test deep links")
(test-begin deep-link)

; Splatter some atoms into the various spaces.
(cog-set-atomspace! base-space)
(Concept "foo" (ctv 1 0 3))

(cog-set-atomspace! mid1-space)
(Concept "bar" (ctv 1 0 4))

(cog-set-atomspace! mid2-space)
(ListLink (Concept "foo") (Concept "bar") (ctv 1 0 5))

; Even though we work on the current surface, we expect to find
; the deeper ListLink.
(cog-set-atomspace! surface-space)
(define lilly (ListLink (Concept "foo") (Concept "bar")))

(test-equal "link-space" mid2-space (cog-atomspace lilly))
(test-equal "foo-space" base-space (cog-atomspace (gar lilly)))
(test-equal "bar-space" mid1-space (cog-atomspace (gdr lilly)))

(test-end deep-link)

; -------------------------------------------------------------------
(opencog-test-end)
