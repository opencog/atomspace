;
; cover-delete-test.scm
; Verify that deleted atoms will cover underlying atoms in nested
; atomspaces.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(opencog-test-runner)

(define pk (Predicate "foobar"))
(define (set-cnt! ATOM FV) (cog-set-value! ATOM pk FV))
(define (get-cnt ATOM) (inexact->exact (cog-value-ref (cog-value ATOM pk) 2)))

; -------------------------------------------------------------------
; Common setup, used by all tests.

(define base-space (cog-atomspace))
(define mid1-space (AtomSpace base-space))
(define mid2-space (AtomSpace mid1-space))
(define mid3-space (AtomSpace mid2-space))
(define surface-space (AtomSpace mid3-space))

; ===================================================================
; Test that deep deletions work correctly.

(define deep-delete "test deep delete")
(test-begin deep-delete)

; Repeatedly add and remove the same atom
(cog-set-atomspace! base-space)
(set-cnt! (Concept "foo") (FloatValue 1 0 3))

(cog-set-atomspace! mid1-space)
(cog-extract! (Concept "foo"))

(cog-set-atomspace! mid2-space)
(set-cnt! (Concept "foo") (FloatValue 1 0 5))

(cog-set-atomspace! mid3-space)
(cog-extract! (Concept "foo"))

(cog-set-atomspace! surface-space)
(set-cnt! (Concept "foo") (FloatValue 1 0 7))

; -----------------------------------
; Should be present in the base space.
(cog-set-atomspace! base-space)
(test-assert "base-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "base-tv" 3 (inexact->exact (get-cnt (cog-node 'Concept "foo"))))

; Should be absent in the next level.
(cog-set-atomspace! mid1-space)
(test-assert "mid1-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! mid2-space)
(test-assert "mid2-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "mid2-tv" 5 (inexact->exact (get-cnt (cog-node 'Concept "foo"))))

(cog-set-atomspace! mid3-space)
(test-assert "mid3-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! surface-space)
(test-assert "surface-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "surface-tv" 7 (inexact->exact (get-cnt (cog-node 'Concept "foo"))))

(test-end deep-delete)

; ===================================================================
; Building on the above, verify that values work

(define deep-change "test deep change-delete")
(test-begin deep-change)

; Repeatedly add and remove the same atom
(cog-set-atomspace! base-space)
(set-cnt! (Concept "foo") (FloatValue 1 0 2))

(cog-set-atomspace! mid2-space)
(set-cnt! (Concept "foo") (FloatValue 1 0 4))

(cog-set-atomspace! surface-space)
(set-cnt! (Concept "foo") (FloatValue 1 0 6))

; -----------------------------------
; Should be present in the base space.
(cog-set-atomspace! base-space)
(test-assert "base-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "base-tv" 2 (inexact->exact (get-cnt (cog-node 'Concept "foo"))))

; Should be absent in the next level.
(cog-set-atomspace! mid1-space)
(test-assert "mid1-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! mid2-space)
(test-assert "mid2-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "mid2-tv" 4 (inexact->exact (get-cnt (cog-node 'Concept "foo"))))

(cog-set-atomspace! mid3-space)
(test-assert "mid3-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! surface-space)
(test-assert "surface-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "surface-tv" 6 (inexact->exact (get-cnt (cog-node 'Concept "foo"))))

(test-end deep-change)

; ===================================================================
; Test that deep link deletions work correctly.

(define deep-link-delete "test deep link-delete")
(test-begin deep-link-delete)

; Repeatedly add and remove the same atom
(cog-set-atomspace! base-space)
(Concept "bar")
(set-cnt! (ListLink (Concept "foo") (Concept "bar")) (FloatValue 1 0 10))

(cog-set-atomspace! mid1-space)
(cog-extract-recursive! (Concept "foo"))

(cog-set-atomspace! mid2-space)
(set-cnt! (ListLink (Concept "foo") (Concept "bar")) (FloatValue 1 0 20))

(cog-set-atomspace! mid3-space)
(cog-extract-recursive! (Concept "foo"))

(cog-set-atomspace! surface-space)
(set-cnt! (ListLink (Concept "foo") (Concept "bar")) (FloatValue 1 0 30))

; -----------------------------------
; Should be present in the base space.
(cog-set-atomspace! base-space)
(test-assert "base-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "base-tv" 2 (inexact->exact (get-cnt (Concept "foo"))))
(test-equal "base-litv" 10 (inexact->exact (get-cnt
    (ListLink (Concept "foo") (Concept "bar")))))

; Should be absent in the next level.
(cog-set-atomspace! mid1-space)
(test-assert "mid1-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! mid2-space)
(test-assert "mid2-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "mid2-tv" 4 (inexact->exact (get-cnt (Concept "foo"))))
(test-equal "mid2-litv" 20 (inexact->exact (get-cnt
    (ListLink (Concept "foo") (Concept "bar")))))

(cog-set-atomspace! mid3-space)
(test-assert "mid3-space" (nil? (cog-node 'Concept "foo")))

(cog-set-atomspace! surface-space)
(test-assert "surface-space" (cog-atom? (cog-node 'Concept "foo")))
(test-equal "surface-tv" 6 (inexact->exact (get-cnt (Concept "foo"))))
(test-equal "surface-litv" 30 (inexact->exact (get-cnt
    (ListLink (Concept "foo") (Concept "bar")))))

(test-end deep-link-delete)

; ===================================================================
(opencog-test-end)
