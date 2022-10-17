;
; recover-test.scm
; Test ability to delete and re-add atoms in different layers.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog test-runner))

(opencog-test-runner)

; -------------------------------------------------------------------
; Common setup, used by all tests.

(define (setup)
	(define base-space (cog-atomspace))
	(define mid1-space (cog-new-atomspace base-space))
	(define mid2-space (cog-new-atomspace mid1-space))
	(define mid3-space (cog-new-atomspace mid2-space))
	(define mid4-space (cog-new-atomspace mid3-space))
	(define mid5-space (cog-new-atomspace mid4-space))
	(define top-space (cog-new-atomspace mid5-space))

	; Splatter some atoms into the various spaces.
	(cog-set-atomspace! base-space)
	(Concept "even")

	(cog-set-atomspace! mid1-space)
	(Concept "odd")

	(cog-set-atomspace! mid2-space)
	(cog-extract! (Concept "even"))

	(cog-set-atomspace! mid3-space)
	(cog-extract! (Concept "odd"))

	(cog-set-atomspace! mid4-space)
	(Concept "even")

	(cog-set-atomspace! mid5-space)
	(Concept "odd")

	; Leave it at the top
	(cog-set-atomspace! top-space)
)

; -------------------------------------------------------------------
; Test that atoms appear and disappear where they should.

(define (test-recover)
	(setup)

	(define top-space (cog-atomspace))

	; Grab references into the inheritance hierarchy
	(define mid5-space (cog-outgoing-atom top-space 0))
	(define mid4-space (cog-outgoing-atom mid5-space 0))
	(define mid3-space (cog-outgoing-atom mid4-space 0))
	(define mid2-space (cog-outgoing-atom mid3-space 0))
	(define mid1-space (cog-outgoing-atom mid2-space 0))
	(define base-space (cog-outgoing-atom mid1-space 0))

	; Verify that atoms appear and disappear properly.
	(cog-set-atomspace! base-space)
	(test-assert "yes-even-0" (not (nil? (cog-node 'Concept "even"))))
	(test-assert "no-odd-0" (nil? (cog-node 'Concept "odd")))
	(test-equal "even-0 as" base-space (cog-atomspace (cog-node 'Concept "even")))

	(cog-set-atomspace! mid1-space)
	(test-assert "yes-even-1" (not (nil? (cog-node 'Concept "even"))))
	(test-assert "yes-odd-1" (not (nil? (cog-node 'Concept "odd"))))
	(test-equal "even-1 as" base-space (cog-atomspace (cog-node 'Concept "even")))
	(test-equal "odd-1 as" mid1-space (cog-atomspace (cog-node 'Concept "odd")))

	(cog-set-atomspace! mid2-space)
	(test-assert "no-even-2" (nil? (cog-node 'Concept "even")))
	(test-assert "yes-odd-2" (not (nil? (cog-node 'Concept "odd"))))
	(test-equal "odd-2 as" mid1-space (cog-atomspace (cog-node 'Concept "odd")))

	(cog-set-atomspace! mid3-space)
	(test-assert "no-even-3" (nil? (cog-node 'Concept "even")))
	(test-assert "no-odd-3" (nil? (cog-node 'Concept "odd")))

	(cog-set-atomspace! mid4-space)
	(test-assert "yes-even-4" (not (nil? (cog-node 'Concept "even"))))
	(test-assert "no-odd-4" (nil? (cog-node 'Concept "odd")))
	(test-equal "even-4 as" mid4-space (cog-atomspace (cog-node 'Concept "even")))

	(cog-set-atomspace! mid5-space)
	(test-assert "yes-even-5" (not (nil? (cog-node 'Concept "even"))))
	(test-assert "yes-odd-5" (not (nil? (cog-node 'Concept "odd"))))
	(test-equal "even-5 as" mid4-space (cog-atomspace (cog-node 'Concept "even")))
	(test-equal "odd-5 as" mid5-space (cog-atomspace (cog-node 'Concept "odd")))

	(cog-set-atomspace! top-space)
	(test-assert "yes-even-6" (not (nil? (cog-node 'Concept "even"))))
	(test-assert "yes-odd-6" (not (nil? (cog-node 'Concept "odd"))))
	(test-equal "even-6 as" mid4-space (cog-atomspace (cog-node 'Concept "even")))
	(test-equal "odd-6 as" mid5-space (cog-atomspace (cog-node 'Concept "odd")))
)

(define recover "test recover atoms")
(test-begin recover)
(test-recover)
(test-end recover)

; ===================================================================
(opencog-test-end)
