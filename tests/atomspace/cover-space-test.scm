;
; cover-space-test.scm
; Verify that links are correctly covered/uncovered in deeply nested
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
(define surface-space (cog-new-atomspace mid2-space))

; Splatter some atoms into the various spaces.
(cog-set-atomspace! base-space)
(Concept "foo" (ctv 1 0 3))

(cog-set-atomspace! mid1-space)
(Concept "bar" (ctv 1 0 4))

(cog-set-atomspace! mid2-space)
(ListLink (Concept "foo") (Concept "bar") (ctv 1 0 5))

(cog-set-atomspace! surface-space)
(List (Concept "foo") (Concept "x"))
(Set (Concept "foo") (Concept "s"))

; -------------------------------------------------------------------
; Test that deep links are found correctly.

(define deep-link "test deep links")
(test-begin deep-link)

; Even though we work on the current surface, we expect to find
; the deeper ListLink.
(cog-set-atomspace! surface-space)
(define lilly (ListLink (Concept "foo") (Concept "bar")))

(test-equal "link-space" mid2-space (cog-atomspace lilly))
(test-equal "foo-space" base-space (cog-atomspace (gar lilly)))
(test-equal "bar-space" mid1-space (cog-atomspace (gdr lilly)))

(test-end deep-link)

; -------------------------------------------------------------------
; Test that deep links are covered correctly.

(define deep-cover "test deep covering")
(test-begin deep-cover)

; Alter counts in the outgoing set.
; The new ListLink should pick up the counts on the deeper one.
(Concept "foo" (ctv 1 0 9))
(define lifnd (cog-link 'ListLink (Concept "foo") (Concept "bar")))
(define litop (ListLink (Concept "foo") (Concept "bar")))

; The mid-grade ListLink should be unchanged.
(test-equal "old-link" lifnd lilly)
(test-equal "link-space" mid2-space (cog-atomspace lilly))
(test-equal "foo-space" base-space (cog-atomspace (gar lilly)))
(test-equal "bar-space" mid1-space (cog-atomspace (gdr lilly)))

; TV should be as before.
(test-equal "link-tv" 5 (get-cnt lilly))
(test-equal "foo-tv" 3 (get-cnt (gar lilly)))
(test-equal "bar-tv" 4 (get-cnt (gdr lilly)))

; The top-most ListLink should be different.
(test-equal "link-top" surface-space (cog-atomspace litop))
(test-equal "foo-top" surface-space (cog-atomspace (gar litop)))
(test-equal "bar-top" mid1-space (cog-atomspace (gdr litop)))

; Verify that the count on ListLink was copied over correctly
(test-equal "top-link-tv" 5 (get-cnt litop))
(test-equal "top-foo-tv" 9 (get-cnt (gar litop)))
(test-equal "top-bar-tv" 4 (get-cnt (gdr litop)))

(test-end deep-cover)

; -------------------------------------------------------------------
; Test that incoming sets to deep links work correctly.

(define deep-inco "test deep incoming")
(test-begin deep-inco)
(cog-set-atomspace! surface-space)

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

(test-end deep-inco)

; -------------------------------------------------------------------
(opencog-test-end)
