;
; flow-formulas.scm -- Dynamically changing flows.
;
; The concept of a "value flow" is the idea that a Value can change
; dynamically, recomputed from a formula that draws on it's inputs.
; Examples of such formulas are provided below, together with the
; code for wiring them into Atoms.
;
; The FormulaStream is a kind of FloatValue, such that, every time that
; it is accessed, the current value -- that is, the current vector of
; floating point numbers -- is recomputed.  The recomputation occurs
; every time the numeric value is accessed.
;
; A more complex demo is in the `flow-futures.scm` file.

(use-modules (opencog) (opencog exec))

; Atomese is verbose, and this demo is easier to understand if some
; of that is hidden a bit. So, define two scheme functions that get
; the strength and confidence of a SimpleTruthValue.
(define tvkey (Predicate "*-TruthValueKey-*"))
(define (strength-of ATOM) (ElementOf (Number 0) (ValueOf ATOM tvkey)))
(define (confidence-of ATOM) (ElementOf (Number 1) (ValueOf ATOM tvkey)))

(cog-set-value! (Concept "A") tvkey (FloatValue 0.9 0.1))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.8 0.2))

; The FormulaStream is a kind of FloatValue that is recomputed, every
; time it is accessed. Thus, it is a kind of dynamically-changing Value.
; It is used here to define a dynamically-changing TruthValue.
; In the following, the pair of number (1-sA*sB, cA*cB) is computed.
(define tv-stream
	(FormulaStream
		(Minus
			(Number 1)
			(Times
				(strength-of (Concept "A"))
				(strength-of (Concept "B"))))
		(Times
			(confidence-of (Concept "A"))
			(confidence-of (Concept "B")))))

; Print it out. Notice a sampling of the current numeric value, printed
; at the bottom. Of course, at this point Concept A and B only have the
; default TV of (1, 0), and so the computed value should be (0, 0).
(display tv-stream) (newline)

; The numeric values only, are printed in a shorter, more readable
; fashion:
(cog-value->list tv-stream)

; When the inputs change, the value will track:
(cog-set-value! (Concept "A") tvkey (FloatValue 0.9 0.2))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.4 0.7))
(cog-value->list tv-stream)

(cog-set-value! (Concept "A") tvkey (FloatValue 0.5 0.8))
(cog-value->list tv-stream)

(cog-set-value! (Concept "B") tvkey (FloatValue 0.314159 0.9))
(cog-value->list tv-stream)

; ----------
; The above example hard-codes the Atoms to be used in the formula.
; It is often convenient to use variables, so that a formula definition
; can be reused.  Thus, lets recycle a portion of the `formulas.scm`
; example and create a formula for computing a SimpleTruthValue, based
; on two input Atoms.
(DefineLink
	(DefinedSchema "has a reddish color")
	(Lambda
		(VariableList (Variable "$X") (Variable "$Y"))
		(FloatColumn
			(Minus
				(Number 1)
				(Times
					(strength-of (Variable "$X"))
					(strength-of (Variable "$Y"))))
			(Times
				(confidence-of (Variable "$X"))
				(confidence-of (Variable "$Y"))))))

; Note that LambdaLink is a link type; it computes the same things as
; the FormulaStream, except that ... it is not a Value! It's a Link.

; Create an EexecutionOutputLink that will apply the formula above to
; a pair of Atoms. This is as before; see the `formulas.scm` example
; for details.
(define exolnk
	(ExecutionOutput
		(DefinedSchema "has a reddish color")
		(List (Concept "A") (Concept "B"))))

; As in earlier examples, the TV on the EvaluationLink is recomputed
; every time that it is evaluated. We repeat this experiment here.
(cog-set-value! (Concept "A") tvkey (FloatValue 0.3 0.7))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.4 0.6))
(cog-execute! exolnk)

; Now that we've verified that the ExecutionOutputLink works as expected,
; it can be deployed in the stream.
(define ex-stream (FormulaStream exolnk))

; Print it out. Notice a sampling of the current numeric value, printed
; at the bottom:
(display ex-stream) (newline)

; Change one of the inputs, and notice the output tracks:
(cog-set-value! (Concept "A") tvkey (FloatValue 0.9 0.2))
(cog-value->list ex-stream)

(cog-set-value! (Concept "A") tvkey (FloatValue 0.5 0.8))
(cog-value->list ex-stream)

(cog-set-value! (Concept "B") tvkey (FloatValue 0.314159 0.9))
(cog-value->list ex-stream)

; ----------
; This dynamic Value becomes interesting when it is used to
; automatically maintain the TV of some relationship. Suppose
; that A implied B, and the truth-probability of this is given
; by the formula above.

; Yes, we can. Just use the PromiseLink.  This can wrap any executable
; Atom, anything that can produce a Value, and provides a promise that
; it can be evaluated in the future. Then, when the SetValueLink is
; executed, whatever was wrapped is unwrapped and placed into a
; FormulaStream, which will then update every time it is accessed.

; This can be used as anywhere any other predicate can be used;
; anywhere a PredicateNode, GroundedPredicateNode, DefinedPredicate,
; or FormulaPredicate can be used. They all provide the same utility:
; they provide a TruthValue. More precisely, a FormulaStream is
; created. This wraps the 2nd and later args to the SetValue.
; This FormulaStream is installed onto the first arg (the
; ImplicationLink). From thenceforth, any calls to get the TV
; on the ImplicatioLink get the FormulaStream, which recomputes
; the TV value each time it's accessed.
;
(define a-implies-b (Implication (Concept "A") (Concept "B")))
(cog-execute!
	(SetValue
		a-implies-b
		tvkey
		(ExecutionOutput
			(DefinedSchema "has a reddish color")
			(List (Concept "A") (Concept "B")))))

; Double-check, as before:
(cog-tv a-implies-b)

; Change the TV on A and B ...
(cog-set-value! (Concept "A") tvkey (FloatValue 0.1 0.9))
(cog-set-value! (Concept "B") tvkey (FloatValue 0.1 0.9))

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
(define fstream (FormulaStream (Plus (Number 10) (FloatValueOf foo akey))))

; Place it on an atom, take a look at it, and make sure that it works.
(cog-set-value! bar bkey fstream)
(cog-value bar bkey)
(cog-execute! (StreamValueOf bar bkey))
(cog-execute! (StreamValueOf bar bkey))
(cog-execute! (StreamValueOf bar bkey))

; ------- THE END -------
