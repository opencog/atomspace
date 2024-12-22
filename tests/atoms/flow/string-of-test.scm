;
; string-of-test.scm -- Test the StringOfLink for basic fuction
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "string-of-test")
(test-begin tname)
; -----------

(define node-from-node
	(cog-execute! (StringOf (Type 'Concept) (Predicate "bar"))))
(format #t "Node from node got ~A\n" node-from-node)

(test-assert "Node-from-node"
	(equal? node-from-node (Concept "bar")))

; -----------

(cog-set-value! (Anchor "anch") (Predicate "key")
	(StringValue "a" "b" "c"))

(define node-from-string
	(cog-execute! (StringOf (Type 'Concept)
		(ValueOf (Anchor "anch") (Predicate "key")))))

(format #t "Node from string got ~A\n" node-from-string)

(test-assert "Node-from-string"
	(equal? node-from-string (Concept "a")))

; -----------

(cog-execute!
	(SetValue (Anchor "anch") (Predicate "strkey")
		(StringOf (Type 'StringValue)
			(Concept "do-da"))))

(define string-from-node
	(cog-value (Anchor "anch") (Predicate "strkey")))
(format #t "Got string from node ~A\n" string-from-node)

(test-assert "string-from-node"
	(equal? string-from-node (StringValue "do-da")))

; -----------

(cog-set-value! (Anchor "anch") (Predicate "flokey")
	(StringValue "scoobey"))

(define filter-string
	(Filter
		(Rule
			(Variable "$strv")
			(Variable "$strv")
			(Edge (Predicate "foobar")
				(List
					(StringOf (Type 'Concept)
						(ValueOf (Variable "$strv"))))))
		(ValueOf (Anchor "anch") (Predicate "flokey"))))

(define flow-string
	(cog-execute! filter-string))

(format #t "Flow string got ~A\n" flow-string)

(test-assert "flow-string"
	(equal? flow-string
		(Edge (Predicate "foobar")
			(List (Concept "scoobey")))))

; -----------
(test-end tname)
(opencog-test-end)
