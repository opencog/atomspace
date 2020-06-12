;
; quote-scope.scm
;
; This shows up in the URE, as a meta-pattern searching for patterns.
;
(use-modules (opencog) (opencog exec))

(ImplicationScope
	(TypedVariable (Variable "X") (Type 'Concept))
	(Member (Variable "X") (Concept "foo"))
	(Member (Concept "bar") (Variable "X")))

(define quote-scope
	(Meet
		(VariableList
			(Variable "$P")
			(Variable "$Q")
			(TypedVariable
				(Variable "$TyVs")
				(TypeChoice (Type "TypedVariable") (Type "VariableList"))))
		(Present
			(Quote
				(ImplicationScope
					(Unquote (Variable "$TyVs"))
					(Unquote (Variable "$P"))
					(Unquote (Variable "$Q")))))))

; (cog-execute! quote-scope)

(define expect
	(List
		(Member (Variable "X") (Concept "foo"))
		(Member (Concept "bar") (Variable "X"))
		(TypedVariable (Variable "X") (Type 'Concept))))
