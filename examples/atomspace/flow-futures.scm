;
; flow-futures.scm -- Dynamically changing FloatValue flows.
;
; The `flow-formulas.scm` demo showed how to attach dynamically-updating
; TruthValues to Atoms. This demo is similar, except that it works with
; general FloatValues.  The specific example here computes the mutual
; information of an ordered pair, given only counts on the pair, and
; counts on the marginals.  This requires doing arithmetic on numbers
; coming from four different places, and then placing the result where
; it can be found.
;
; This is a fairly complex demo, as it attempts to be more realistic.
;
; As before, the core function is provided by the FormulaStream, which
; which wraps an arithmetic expression so that it behaves like a future.
; See https://en.wikipedia.org/wiki/Futures_and_promises for the general
; idea.

(use-modules (opencog) (opencog exec))

; -------------------------------------------------------------

; ------- THE END -------
