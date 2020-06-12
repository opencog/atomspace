;
; constant-present.scm
;
; Contains a constant clause insidde of a PresentLink.
; This occurs naturally in the URE ForwardChainerUTest
;

(use-modules (opencog) (opencog exec))

(Inheritance (Concept "B") (Concept "foo"))

(define query
	(Bind
		(TypedVariable (Variable "$C-7a4842c1") (Type "Concept"))
		(And
			(Present
				(Inheritance (Concept "A") (Concept "B"))
				(Inheritance (Concept "B") (Variable "$C-7a4842c1")))
			(Not (Identical (Variable "$C-7a4842c1") (Concept "A"))))
		(Execution
			(Schema "scm: fc-deduction-formula")
			(List
				(Inheritance (Concept "A") (Variable "$C-7a4842c1"))
				(Inheritance (Concept "A") (Concept "B"))
				(Inheritance (Concept "B") (Variable "$C-7a4842c1"))))))

; (cog-execute! query)

(define expected
	(Set
		(Execution
			(Schema "scm: fc-deduction-formula")
			(List
				(Inheritance (Concept "A") (Concept "foo"))
				(Inheritance (Concept "A") (Concept "B"))
				(Inheritance (Concept "B") (Concept "foo"))))))
