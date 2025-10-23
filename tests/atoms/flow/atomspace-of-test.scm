;
; atomspace-of-test.scm -- Verify that AtomSpaceOfLink works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "atomspace-of-test")
(test-begin tname)

; Get the current AtomSpace
(define main-as (cog-atomspace))

; Create an atom in the main AtomSpace
(define test-atom (Concept "test-atom"))

; Test AtomSpaceOfLink with atom in current AtomSpace
(define as-result (cog-execute! (AtomSpaceOf test-atom)))

; Verify the result is an AtomSpace
(test-assert "result-is-atomspace"
	(cog-atom? as-result))

(test-assert "result-type-is-atomspace"
	(equal? (cog-type as-result) 'AtomSpace))

; Verify the returned AtomSpace is the same as the main one
(test-assert "same-atomspace"
	(equal? as-result main-as))

; Create a child AtomSpace
(define child-as (cog-new-atomspace main-as))

; Switch to child AtomSpace and create an atom there
(cog-set-atomspace! child-as)
(define child-atom (Concept "child-atom"))

; Test that child-atom returns child-as
(define child-as-result (cog-execute! (AtomSpaceOf child-atom)))
(test-assert "child-atomspace"
	(equal? child-as-result child-as))

; Test with an atom from the parent (visible in child)
; The atom should still belong to main-as, not child-as
(define parent-as-result (cog-execute! (AtomSpaceOf test-atom)))
(test-assert "parent-atomspace-from-child"
	(equal? parent-as-result main-as))

; Verify they are different
(test-assert "different-atomspaces"
	(not (equal? child-as main-as)))

; Test with executable argument that returns an atom
(define test-link (List (Concept "foo") (Concept "bar")))
(define link-as-result (cog-execute! (AtomSpaceOf (ValueOf test-link (Predicate "some-key")))))

; Since ValueOf might return an atom, AtomSpaceOf should handle it
; If ValueOf returns an undefined value (no such key), AtomSpaceOf should return undefined
; Let's test with a simpler executable
(define quoted-atom (Quote test-atom))
(define quoted-result (cog-execute! (AtomSpaceOf quoted-atom)))
; Quote is executable and returns test-atom, so result should be main-as
(test-assert "executable-argument"
	(equal? quoted-result main-as))

; Switch back to main AtomSpace
(cog-set-atomspace! main-as)

; Test AtomSpaceOf on the child AtomSpace itself
; The child AtomSpace is an atom too, so it should have an AtomSpace
(define child-as-location (cog-execute! (AtomSpaceOf child-as)))
; The child AtomSpace atom should be in the main AtomSpace
(test-assert "atomspace-in-atomspace"
	(equal? child-as-location main-as))

(test-end tname)

(opencog-test-end)
