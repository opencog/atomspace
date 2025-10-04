;
; keys-of-test.scm -- Verify that KeysOfLink works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "keys-of-test")
(test-begin tname)

; Create an atom and attach some values with different keys
(define test-atom (Concept "test-atom"))

(define key1 (Predicate "key-one"))
(define key2 (Predicate "key-two"))
(define key3 (Predicate "key-three"))

; Attach values using different keys
(cog-set-value! test-atom key1 (FloatValue 1.0 2.0 3.0))
(cog-set-value! test-atom key2 (StringValue "hello" "world"))
(cog-set-value! test-atom key3 (FloatValue 42.0))

; Test KeysOfLink with single atom
(define keys-result (cog-execute! (KeysOf test-atom)))

; We expect 3 keys
(test-assert "keys-count"
	(equal? (cog-arity keys-result) 3))

; Verify the keys are present in the result
; Convert to a set for easier comparison
(define keys-set (cog-execute! (CollectionOf (KeysOf test-atom))))
(test-assert "keys-content"
	(equal? keys-set (Set key1 key2 key3)))

; Test KeysOfLink with multiple atoms
(define test-atom2 (Concept "test-atom-2"))
(define key4 (Predicate "key-four"))

(cog-set-value! test-atom2 key2 (FloatValue 100.0))
(cog-set-value! test-atom2 key4 (StringValue "foo"))

; Get keys from both atoms
(define all-keys (cog-execute! (KeysOf test-atom test-atom2)))

; We expect 4 unique keys (key1, key2, key3, key4)
; Note: key2 appears on both atoms but should only appear once
(test-assert "multi-keys-count"
	(equal? (cog-arity all-keys) 4))

(define all-keys-set (cog-execute! (CollectionOf (KeysOf test-atom test-atom2))))
(test-assert "multi-keys-content"
	(equal? all-keys-set (Set key1 key2 key3 key4)))

(test-end tname)

(opencog-test-end)
