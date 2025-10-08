;
; value-of.scm --  Searching for patterns with attached values.
;
; A common task is to search for patterns that have a sufficiently
; large truth value or attention value (or some other value). This
; can be acheived by using a combination of ElementOf and ValueOf
; to select the desired value, and then using GreaterThan to compare.
;

(use-modules (opencog) (opencog exec))

(define tvkey (Predicate "*-TruthValueKey-*"))
(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))
(define (confidence-of ATOM) (ElementOf (Number 1) (ValueOf ATOM tvkey)))

; Some data.
(cog-set-value! (Concept "is mostly true") tvkey (FloatValue 0.9 0.9))
(cog-set-value! (Concept "is mostly false") tvkey (FloatValue 0.234 0.9))

; Define a pattern that will only find ConceptNodes that have
; a low truth value.
(define find-false
	(Query
		; Search only for ConceptNodes.
		(TypedVariable (Variable "$X") (Type 'ConceptNode))
		(And
			(Present (Variable "$X"))
			; Want the strength of the TV to be less than half.
			(GreaterThan (Number 0.5) (strength-of (Variable "$X"))))
		(Variable "$X")))

; Run it. Lo and Behold!
(cog-execute! find-false)

; --------------
; Create some goofy numbers
(define key (Predicate "some key"))

(cog-set-value! (Concept "thing-a") key (FloatValue 42))
(cog-set-value! (Concept "thing-b") key (FloatValue 35))

(define find-answer
	(Query
		; Search only for ConceptNodes.
		(TypedVariable (Variable "$X") (Type 'ConceptNode))
		(And
			(Present (Variable "$X"))
			; Divide by twelve, then see if its more than 3.
			(GreaterThan
				(Divide
					(FloatValueOf (Variable "$X") key)
					(Number 12))
				(Number 3)))
		(Variable "$X")))

(cog-execute! find-answer)
