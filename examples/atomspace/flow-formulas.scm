;
; flow-formulas.scm -- Dynamically changing flows.
;
; The concept of a "value flow" is the idea that a Value can change
; dynamically, recomputed from a formula that draws on it's inputs.
; Examples of such formulas are provided below, together with the
; code for wiring them into Atoms.
;
; The core implementation is in two parts: the FormulaTruthValue,
; which implements a dynamically-variable TruthValue, and the
; DynamicPredicateLink, which installs this TruthValue into an Atom.
;
; The FormulaTruthValue is a kind of SimpleTruthValue, such that, every
; time that it is accessed, the current value -- that is, the current
; pair of floating point numbers -- is recomputed.  The recomputation
; occurs every time the numeric value is accessed (i.e. when the
; strength and confidence of the TV are accessed).
;
; The FormulaStream is a generalization of the FormulaTruthValue, in
; that it allows for the computation of any FloatValue. That is, the
; SimpleTV's are just vectors of length two - the strength and
; confidence, whereas the FloatValue is a vector of arbitrary length.

(use-modules (opencog) (opencog exec))

; The FormulaTruthValue is a kind of TruthValue that is recomputed,
; every time it is accessed. Thus, it is a kind of dynamically-changing
; TruthValue. The value to be computed can be defined in Atomese. Thus,
; in the following, the SimpleTV of (1-sA*sB, cA*cB) is computed.
(define tv-stream
	(FormulaTruthValue
		(FormulaPredicate
			(Minus
				(Number 1)
				(Times
					(StrengthOf (Concept "A"))
					(StrengthOf (Concept "B"))))
			(Times
				(ConfidenceOf (Concept "A"))
				(ConfidenceOf (Concept "B"))))))

; Print it out. Notice a sampling of the current numeric value, printed
; at the bottom. Of course, at this point Concept A and B only have the
; default TV of (1, 0), and so the computed value should be (0, 0).
(display tv-stream) (newline)

; The numeric values only, are printed in a shorter, more readable
; fashion:
(cog-value->list tv-stream)

; When the inputs change, the value will track:
(cog-set-tv! (Concept "A") (stv 0.9 0.2))
(cog-set-tv! (Concept "B") (stv 0.4 0.7))
(cog-value->list tv-stream)

(cog-set-tv! (Concept "A") (stv 0.5 0.8))
(cog-value->list tv-stream)

(cog-set-tv! (Concept "B") (stv 0.314159 0.9))
(cog-value->list tv-stream)

; ----------
; The above example hard-codes the Atoms to be used in the formula.
; It is often convenient to use variables, so that a formula definition
; can be reused.  Thus, lets recycle a portion of the `formulas.scm`
; example and create a formula for computing a SimpleTruthValue, based
; on two input Atoms.
(DefineLink
   (DefinedPredicate "has a reddish color")
   (FormulaPredicate
      (Minus
         (Number 1)
         (Times
            (StrengthOf (Variable "$X"))
            (StrengthOf (Variable "$Y"))))
      (Times
         (ConfidenceOf (Variable "$X"))
         (ConfidenceOf (Variable "$Y")))))

; Create an EvaluationLink that will apply the formula above to a pair
; of Atoms. This is as before; see the `formulas.scm` example for details.
(define evlnk
	(Evaluation
		(DefinedPredicate "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; As in earlier examples, the TV on the EvaluationLink is recomputed
; every time that it is evaluated. We repeat this experiment here.
(cog-set-tv! (Concept "A") (stv 0.3 0.7))
(cog-set-tv! (Concept "B") (stv 0.4 0.6))
(cog-evaluate! evlnk)
(cog-tv evlnk)

; Now that we've verified that the EvaluationLink works as expected,
; it can be deployed in the stream.
(define ev-stream (FormulaTruthValue evlnk))

; Print it out. Notice a sampling of the current numeric value, printed
; at the bottom:
(display ev-stream) (newline)

; Change one of the inputs, and notice the output tracks:
(cog-set-tv! (Concept "A") (stv 0.9 0.2))
(cog-value->list ev-stream)

(cog-set-tv! (Concept "A") (stv 0.5 0.8))
(cog-value->list ev-stream)

(cog-set-tv! (Concept "B") (stv 0.314159 0.9))
(cog-value->list ev-stream)

; ----------
; This new kind of TV becomes interesting when it is used to
; automatically maintain the TV of some relationship. Suppose
; that A implied B, and the truth-probability of this is given
; by the formula above. So, first we write the implication:

(define a-implies-b (Implication (Concept "A") (Concept "B")))

; ... and then attach this auto-updating TV to it.
(cog-set-tv! a-implies-b tv-stream)

; Take a look at it, make sure that it is actually there.
(cog-tv a-implies-b)

; The above printed the "actual" TV, as it sits on the Atom.
; However, typically, we want the numeric values, and not the formula.
; These can be gotten simply by asking for them, directly, by name.
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
; the setting of truth values. Can we do the same, without using scheme?
; Yes, we can. Just use the DynamicPredicateLink.  This is quite similar
; to the FormulaPredicateLink, demoed in `formulas.scm`, but in this
; case, instead of producing a single, static TV, this wraps the entire
; formula into a FormulaTruthValue. Thus, it is enough to set the TV
; only once; after that, the TV updates will be automatic.

; For example:
(cog-execute!
	(SetTV
		(Implication (Concept "A") (Concept "B"))
		(DynamicPredicate
			(Minus
				(Number 1)
				(Times
					(StrengthOf (Concept "A"))
					(StrengthOf (Concept "B"))))
			(Times
				(ConfidenceOf (Concept "A"))
				(ConfidenceOf (Concept "B"))))))

; The above can be tedious, as it requires manually creating a new
; formula for each SetTV.  Some of this tedium can be avoided by
; using formulas with variables in them. Using the same formula as
; before, we get a dynamic example:
(DefineLink
   (DefinedPredicate "dynamic example")
   (FormulaPredicate
      (Minus
         (Number 1)
         (Times
            (StrengthOf (Variable "$X"))
            (StrengthOf (Variable "$Y"))))
      (Times
         (ConfidenceOf (Variable "$X"))
         (ConfidenceOf (Variable "$Y")))))

; This can be used as anywhere any other predicate can be used;
; anywhere a PredicateNode, GroundedPredicateNode, DefinedPredicate,
; or FormulaPredicate can be used. They all provide the same utility:
; they provide a TruthValue.
(cog-execute!
	(SetTV
		(Implication (Concept "A") (Concept "B"))
		(DefinedPredicate "dynamic example")
		(Concept "A") (Concept "B")))

; Double-check, as before:
(cog-tv a-implies-b)

; Change the TV on A and B ...
(cog-set-tv! (Concept "A") (stv 0.1 0.9))
(cog-set-tv! (Concept "B") (stv 0.1 0.9))

; And take another look.
(format #t "A implies B has strength ~6F and confidence ~6F\n"
	(cog-mean a-implies-b) (cog-confidence a-implies-b))

; -------------------------------------------------------------
; The FormulaStream is the generalization of FormulaTruthValue, suitable
; for streaming a FloatValue of arbitrary length. As before, whenever it
; is accessed, the current vector value is recomputed. The recomputation
; forced by calling `execute()` on the Atom that the stream is created
; with.
;
; Create an Atom, a key, and a random stream of five numbers.
; The random stream is a FloatValue vector, of length 5; each of
; the numbers are randomly distributed between 0.0 and 1.0
(define foo (Concept "foo"))
(define bar (Concept "bar"))
(define akey (Predicate "some key"))
(define bkey (Predicate "other key"))

(cog-set-value! foo akey (RandomStream 5))

; Take a look at what was created.
(cog-value foo akey)

; Verify that it really is a vector, and that it changes with each
; access. The StreamValueOfLink will sample from the RandomStream.
(cog-execute! (StreamValueOf foo akey))

; Apply a formula to that stream, to get a different stream.
(define fstream (FormulaStream (Plus (Number 10) (ValueOf foo akey))))

; Place it on an atom, take a look at it, and make sure that it works.
(cog-set-value! bar bkey fstream)
(cog-value bar bkey)
(cog-execute! (StreamValueOf bar bkey))
(cog-execute! (StreamValueOf bar bkey))
(cog-execute! (StreamValueOf bar bkey))

; TODO: At this time, there is no DynamicSchema that would be the
; generalized analog of DynamicPredicate demonstrated above.  This
; should be created...

; ------- THE END -------
