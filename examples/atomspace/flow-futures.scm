;
; flow-futures.scm -- Dynamically changing FloatValue flows.
;
; The concept of a "value flow" is the idea that a Value can change
; dynamically, recomputed from a formula that draws on it's inputs.
; Examples of such formulas are provided below, together with the
; code for wiring them into Atoms.
;
; The core implementation is in two parts: the FutureTruthValue,
; which implements a dynamically-variable TruthValue, and the
; FormulaPredicateLink, which specifies the formula used to compute
; the TruthValue.
;
; The FutureTruthValue is a kind of SimpleTruthValue, such that, every
; time that it is accessed, the current value -- that is, the current
; pair of floating point numbers -- is recomputed.  The recomputation
; occurs every time the numeric value is accessed (i.e. when the
; strength and confidence of the TV are accessed).
;
; The FormulaStream is a generalization of the FutureTruthValue, in
; that it allows for the computation of any FloatValue. That is, the
; SimpleTV's are just vectors of length two - the strength and
; confidence, whereas the FloatValue is a vector of arbitrary length.

(use-modules (opencog) (opencog exec))

; -------------------------------------------------------------
; The FormulaStream is the generalization of FutureTruthValue, suitable
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

; ------- THE END -------
