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
; to be specified in Atomese. Below, simply take the square of the TV.
(cog-execute!
	(SetTV
		(Concept "bar")
		(Times
			(TruthValueOf (Concept "foo"))
			(TruthValueOf (Concept "foo")))))

; Formulas can be used to compute TV's, as shown in the `formula.scm`
; example. Consider a named formula, with variables.
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

; Some data...
(Concept "A" (stv 0.9 0.98))
(Concept "B" (stv 0.9 0.98))

; Use the formula to compute a new TV, and attach that TV to some Atom.
; This is little more than the copy above, except that the Evaluation
; is actually performed, so that the new TV is computed, before being
; copied. In general, if the second Atom passed to SetTV is evaluatable,
; then it will be evaluated to obtain the TV.
(cog-execute!
	(SetTV
		(Concept "bar")
		(Evaluation
			(DefinedPredicate "has a reddish color")
			(List (Concept "A") (Concept "B")))))

; In many ways, the SetTVLink behaves a lot like a generalized
; EvaluationLink. So: normally, an EvaluationLink consists of a
; predicate, and the list of arguments that it applies to. The
; SetTVLink is similar, except that it couples the predicate to
; the target Atom that it should apply to.  This can be seen in
; the equivalent form, below.
(cog-execute!
	(SetTV
		(Concept "bar")
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))
;
; -------- THE END -----------
