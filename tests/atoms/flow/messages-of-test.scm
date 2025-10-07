;
; messages-of-test.scm -- Verify that MessagesOfLink works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "messages-of-test")
(test-begin tname)

; Test MessagesOfLink with single atom
; By default, regular atoms return empty message set
(define test-atom (Concept "test-atom"))

(define messages-result (cog-execute! (MessagesOf test-atom)))

; We expect 0 messages for regular atoms
(test-assert "no-messages-by-default"
	(equal? (cog-arity messages-result) 0))

; Test MessagesOfLink with multiple atoms
; All should return empty sets by default
(define test-atom2 (Concept "test-atom-2"))
(define all-messages (cog-execute! (MessagesOf test-atom test-atom2)))

(test-assert "multi-no-messages"
	(equal? (cog-arity all-messages) 0))

; Verify empty LinkValue is returned
(test-assert "empty-linkvalue"
	(equal? messages-result (LinkValue)))

(test-end tname)

(opencog-test-end)
