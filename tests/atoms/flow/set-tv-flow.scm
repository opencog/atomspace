;
; tv-flow.scm -- Unit test similar to `flows.scm` example.
;
(use-modules (opencog) (opencog exec))

; An atom with a TruthValue on it...
(Concept "foo" (stv 0.3 0.7))

; Transfer the TruthValue from "foo" to "bar" ... copy it.
(define copy-tv
	(SetTV
		(Concept "bar")
		(TruthValueOf (Concept "foo"))))

; Verify that the TV on "bar" has changed.
; (cog-tv (Concept "bar"))

; Take the square of the TV.
(define product
	(SetTV
		(Concept "bar")
		(Times
			(TruthValueOf (Concept "foo"))
			(TruthValueOf (Concept "foo")))))

(DefineLink
   (DefinedPredicate "has a reddish color")
   (PredicateFormula
      (Minus
         (Number 1)
         (Times
            (StrengthOf (Variable "$X"))
            (StrengthOf (Variable "$Y"))))
      (Times
         (ConfidenceOf (Variable "$X"))
         (ConfidenceOf (Variable "$Y")))))

(Concept "A" (stv 0.9 0.98))
(Concept "B" (stv 0.9 0.98))

(define set-eval
	(SetTV
		(Concept "bar")
		(Evaluation
			(DefinedPredicate "has a reddish color")
			(List (Concept "A") (Concept "B")))))

(define set-direct
	(SetTV
		(Concept "bar")
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; -----------------
; GPN test

(define (reddish-tv A B)
	(SimpleTruthValue
		(- 1 (*  (cog-mean A) (cog-mean B)))
		(*  (cog-confidence A) (cog-confidence B))))

(define set-gpn
	(SetTV
		(Concept "martian rock")
		(GroundedPredicate "scm: reddish-tv")
		(List (Concept "A") (Concept "B"))))

(*unspecified*)
