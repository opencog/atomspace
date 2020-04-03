;
; flows.scm
;
;

(use-modules (opencog) (opencog exec))

(Concept "foo" (stv 0.3 0.7))

(cog-evaluate! (TruthValueOf (Concept "foo"))

(cog-execute!
	(SetTV
		(Concept "bar")
		(TruthValueOf (Concept "foo"))))
