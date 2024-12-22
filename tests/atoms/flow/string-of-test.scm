;
; string-of-test.scm
;
(use-modules (opencog) (opencog exec))

(define node-to-node
	(cog-execute! (StringOf (Type 'Concept) (Predicate "bar"))))
(format #t "Got ~A\n" node-to-node)

