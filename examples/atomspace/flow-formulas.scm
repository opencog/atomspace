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

; ----------
; The FormulaTruthValue is a kind of SimpleTruthValue, such that, every
; time that it is accessed, the current value -- that is, the current
; pair of floating point numbers -- is recomputed.  The recomputation
; is forced by calling evaluate on the Atom that the stream is created
; with. In this example, that means that the EvaluationLink, created
; above, will be evaluated, and the result of that evaluation (which
; is a SimpleTruthValue) is taken as the current numeric value of the
; stream. This is illustrated below.
;
; First, create the stream:
(define tv-stream (FormulaTruthValue evlnk))

; Print it out. Notice a sampling of the current numeric value, printed
; at the bottom:
(display tv-stream) (newline)

; Change one of the inputs, and notice the output tracks:
(cog-set-tv! (Concept "A") (stv 0.9 0.2))
(display tv-stream) (newline)

(cog-set-tv! (Concept "A") (stv 0.5 0.8))
(display tv-stream) (newline)

(cog-set-tv! (Concept "B") (stv 0.314159 0.9))
(display tv-stream) (newline)

; ----------
; This new kind of TV becomes interesting when it is used to
; automatically maintain the TV of some relationship. Suppose
; that A implied B, and the truth-probability of this is given
; by the formula above. So, first we write the implication:

(define a-implies-b (Implication (Concept "A") (Concept "B")))

; ... and then attach this auto-updating TV to it.
(cog-set-tv! a-implies-b tv-stream)

; Lets take a look at it's current value:
(format #t "A implies B has strength ~6F and confidence ~6F\n"
	(cog-mean a-implies-b) (cog-confidence a-implies-b))

; Change the TV on A and B ...
(cog-set-tv! (Concept "A") (stv 0.4 0.2))
(cog-set-tv! (Concept "B") (stv 0.7 0.8))

; ... and the TV on the implication stays current.
; Note that a different API is demoed below.
(format #t "A implies B has strength ~6F and confidence ~6F\n"
	(cog-tv-mean (cog-tv a-implies-b))
	(cog-tv-confidence (cog-tv a-implies-b)))

; ----------
; So far, the above is using a lot of scheme scaffolding to accomplish
; the setting of truth values.
(SetTV
	(Implication (Concept "A") (Concept "B"))
	(FormulaTruthValue evlnk))

; -------------------------------------------------------------
; The FormulaStream is the generalization of FormulaTruthValue, suitable
; for streaming a FloatValue of arbitary length. As before, whenever it
; is accessed, the current vector value is recomputed. The recomputation
; forced by calling `execute()` on the Atom that the stream is created
; with.
;
; First, create the stream:
(define ev-stream (FormulaStream evlnk))


; ------- THE END -------
