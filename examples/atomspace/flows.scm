;
; flows.scm -- Flowing Values between Atoms.
;
;

(use-modules (opencog) (opencog exec))

; An atom with a TruthValue on it...
(Concept "foo" (stv 0.3 0.7))

; The TruthValue can be fetched in either of two ways.
(cog-evaluate! (TruthValueOf (Concept "foo")))
(cog-execute!  (TruthValueOf (Concept "foo")))

; Transfer the TruthValue from "foo" to "bar" ... copy it.
(cog-execute!
	(SetTV
		(Concept "bar")
		(TruthValueOf (Concept "foo"))))

; Verify that the TV on "bar" has changed.
(cog-tv (Concept "bar"))

; SetTV is interesting because it allows complex arithmetic expressions
; to be specified in Atomese.
(cog-execute!
	(SetTV
		(Concept "bar")
		(Times
			(TruthValueOf (Concept "foo"))
			(TruthValueOf (Concept "foo")))))
