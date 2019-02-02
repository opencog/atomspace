;
; value-of.scm --  Searching for patterns with attached values.
;
; A common task is to search for patterns that have a sufficiently
; large truth value or attention value (or some other value). The
; various `ValueOfLink`s can be used to apply such limits to searches.

(use-modules (opencog) (opencog exec))

; Some data.
(Concept "is mostly true" (stv 0.9 0.9))
(Concept "is mostly false" (stv 0.234 0.9))

; Define a pattern that will only find ConceptNodes that have
; a low truth value.
(define find-false
	(Bind
		; Search only for concept nodes.
		(TypedVariable (Variable "$X") (Type 'ConceptNode))
		(And
			(Present (Variable "$X"))
			; Want the strength of the TV to be less than half.
			(GreaterThan (Number 0.5) (StrengthOf (Variable "$X"))))
		(Variable "$X")))

; Run it. Lo and Behold!
(cog-execute! find-false)
