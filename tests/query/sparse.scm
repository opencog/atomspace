;
; sparse.scm
;
; This tests some basic ops used in chemistry informatics.
; Basically, 'Concept 'Predicate and 'Any are stand-ins for
; hydrogen, oxygen and carbon in a "real-world" app.
; This test just makes sure that some explicit search for
; some specific functional group actually finds that group.
; This is a very basic test!
;

(use-modules (opencog))
(use-modules (opencog exec))

(define est
	(GetLink
		(VariableList
			(TypedVariable (Variable "$vA") (Type 'Concept))
			(TypedVariable (Variable "$vB") (Type 'Predicate))
			(TypedVariable (Variable "$vC") (Type 'Any))

			(Variable "$vD")
			(Glob "rest"))

		(Unordered
			(Unordered (Variable "$vB") (Variable "$vA"))
			(Unordered (Variable "$vC") (Variable "$vB"))
			(Unordered (Variable "$vD") (Variable "$vC"))
			(Glob "rest"))
	))

(define est-uni
	(GetLink
		(VariableList
			(TypedVariable (Variable "$vA") (Type 'Concept))
			(TypedVariable (Variable "$vB") (Type 'Predicate))
			(TypedVariable (Variable "$vC") (Type 'Any))

			(Variable "$vD")
			(Glob "rest"))

		(And
			(Present
				(Unordered
					(Unordered (Variable "$vB") (Variable "$vA"))
					(Unordered (Variable "$vC") (Variable "$vB"))
					(Unordered (Variable "$vD") (Variable "$vC"))
					(Glob "rest")))
			(Not (Identical (Variable "$vD") (Variable "$vB"))))
	))

(Unordered
	(Unordered (Predicate "B") (Concept "A"))
	(Unordered (Any "C") (Predicate "B"))
	(Unordered (Procedure "D") (Any "C"))
	(Unordered (Anchor "E") (Procedure "D"))
	(Unordered (Anchor "F") (Anchor "E"))
	(Unordered (Anchor "G") (Anchor "F"))
)

; (cog-execute! est)
; ---------------------------------------------------------------------

; Palindrome matching.
; Note that the variable tags differ from the above, and so the
; query below will not match the data above.
(define palin
	(GetLink
		(VariableList
			(TypedVariable (Variable "$vA") (Type 'Predicate))
			(TypedVariable (Variable "$vB") (Type 'Concept))
			(TypedVariable (Variable "$vC") (Type 'Any))

			(Variable "$vD")
			(Glob "rest")
		)

		(Unordered
			(Unordered (Variable "$vB") (Variable "$vA"))
			(Unordered (Variable "$vC") (Variable "$vB"))
			(Unordered (Variable "$vD") (Variable "$vC"))
			(Glob "rest")
		)))

(define palin-disambig
	(GetLink
		(VariableList
			(TypedVariable (Variable "$vA") (Type 'Predicate))
			(TypedVariable (Variable "$vB") (Type 'Concept))
			(TypedVariable (Variable "$vC") (Type 'Any))

			(Variable "$vD")
			(Glob "rest"))

		(And
			(Present
				(Unordered
					(Unordered (Variable "$vB") (Variable "$vA"))
					(Unordered (Variable "$vC") (Variable "$vB"))
					(Unordered (Variable "$vD") (Variable "$vC"))
					(Glob "rest")))
			(Not (Identical (Variable "$vD") (Variable "$vB"))))
	))

(Unordered
	(Unordered (Concept "BB") (Predicate "AA"))
	(Unordered (Any "CC") (Concept "BB"))
	(Unordered (Procedure "DD") (Any "CC"))
	(Unordered (Any "EE") (Procedure "DD"))
	(Unordered (Concept "FF") (Any "EE"))
	(Unordered (Predicate "GG") (Concept "FF"))
)

; ---------------------------------------------------------------------
