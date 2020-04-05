;
; flow-formulas.scm -- Dynamically changing flows.
;

(use-modules (opencog) (opencog exec))

; A formula for computing a SimpleTruthValue, based on two input Atoms.
; See the `formulas.scm` example for a detailed explanation of how
; this should be understood.
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

; Create an EvalationLink that will apply the formula above to a pair
; of Atoms. See the `formulas.scm` example for details.
(define evlnk
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; As in earlier examples, the TV on the EvaluationLink is recomputed
; every time that it is evaluated.
(cog-set-tv! (Concept "A") (stv 0.3 0.7))
(cog-set-tv! (Concept "B") (stv 0.4 0.6))
(cog-evaluate! evlnk)
(cog-tv evlnk)

; The EvaluationStream is a kind of FloatValue, such that, every time
; that it is accessed, the current value -- that is, the current vector
; of floating point numbers -- is recomputed.  The recomputation is
; forced by calling evaluate (or execute, if available) on the Atom
; that the stream is created with. In this example, that means that
; the EvaluationLink, created above, will be evaluated, and the result
; of that evaluation (which is a SimpleTruthValue) is taken as the
; current numeric value of the stream. This is illustrated below.
;
; First, create the stream:
(define evstream (EvaluationStream evlnk))

; Print it out. Notice a sampling of the current numeric value, printed
; at the bottom:
(display evstream)

; Change one of the inputs, and notice the output tracks:
(cog-set-tv! (Concept "A") (stv 0.4 0.7))
(display evstream)

(cog-set-tv! (Concept "A") (stv 0.5 0.8))
(display evstream)

(cog-set-tv! (Concept "B") (stv 0.9 0.9))
(display evstream)


; ------- THE END -------
