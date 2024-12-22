;
; string-of-test.scm -- Test the StringOfLink for basic fuction
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "string-of-test")
(test-begin tname)
; -----------

(define node-to-node
	(cog-execute! (StringOf (Type 'Concept) (Predicate "bar"))))
(format #t "Got ~A\n" node-to-node)

(test-assert "Node-to-node"
	(equal? node-to-node (Concept "bar")))

; -----------
(test-end tname)
(opencog-test-end)
