;
; multi-space-test.scm -- Using multiple AtomSpaces at the same time.
; This is the unit test for the `multi-space.scm` demo.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "multi-space-test")
(test-begin tname)

; Create three AtomSpaces; two singletons, and a third that contains
; the first two.
(define a (AtomSpace))
(define b (AtomSpace))
(define c (AtomSpace a b))

(cog-set-atomspace! a)
(Concept "I'm in A")

(cog-set-atomspace! b)
(Concept "I'm in B")

; Switch to the third AtomSpace.
(cog-set-atomspace! c)

(define get-concepts
	(Get (TypedVariable (Variable "$x") (Type 'Concept))
		(Variable "$x")))

; Run the query. It should find both Concepts. This is, it should
; print out a `SetLink` containing both of the above Concepts.
(define set-of-both (cog-execute! get-concepts))

(test-assert "query for both"
	(equal? set-of-both (Set (Concept "I'm in A") (Concept "I'm in B"))))

; Go back to the first AtomSpace. Verify that it is as we left it.
(cog-set-atomspace! a)
(test-assert "Just atomspace A"
	(equal? (cog-get-atoms 'Concept) (list (ConceptNode "I'm in A"))))

(cog-set-atomspace! b)
(test-assert "Just atomspace B"
	(equal? (cog-get-atoms 'Concept) (list (ConceptNode "I'm in B"))))

(test-end tname)

(opencog-test-end)
