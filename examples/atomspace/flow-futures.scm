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
; Below is a toy pair-counting framework.  It increments counts
; on pairs, as well as on marginals.

(define (incr-counts THING-A THING-B)
	(cog-inc-count! (List THING-A THING-B))
	(cog-inc-count! (List (AnyNode "left wildcard") THING-B))
	(cog-inc-count! (List THING-A (AnyNode "right wildcard")))
	(cog-inc-count! (AnyNode "grand total")))

; Same as above, but works with strings. It counts how often a pair
; was "observed".
(define (observe STRING-A STRING-B)
	(incr-count (Concept STRING-A) (Concept STRING-B)))

; Provide some initial data
(observe "hello" "world")
(observe "hello" "Sue")
(observe "hello" "Adrian")
(observe "goodbye" "Adrian")
(observe "goodbye" "Mike")

; -------------------------------------------------------------
; Set up the pipeline.

; Counts are located in the third slot of the TV predicate.
(define tvp (PredicateNode "*-TruthValueKey-*"))

; We will use the formula MI(a,b) = log_2 N(a,b) N(*,*) / N(a,*) N(*,b)
; to compute the MI of a pair.
(DefineLink
	(DefinedProcedure "dynamic MI")
	(Lambda
		(VariableList (Variable "$L") (Variable "$R"))
		(Log2
			(Divide
				(Times
					(FloatValueOf (List (Variable "$L") (Variable "$R")) tvp)
					(FloatValueOf (AnyNode "grand total") tvp))
				(Times
					(FloatValueOf (List (Variable "$L") (Any "right wildcard")) tvp)
					(FloatValueOf (List (Any "left wildcard") (Variable "$R")) tvp))))))

; A utility to install the above formula on a pair.
; The formula does no good if unless we stick it on the object.
(define (install-formula THING-A THING-B)
	(define pair (List THING-A THING-B))
	(cog-set-value! pair (Predicate "MI Key")
		(FormulaStream
			(ExecutionOutput (DefinedProcedure "dynamic MI") pair))))

; Convenience wrapper, works with strings.
(define (install-mi STRING-A STRING-B)
	(install-formula (Concept STRING-A) (Concept STRING-B)))

; -------------------------------------------------------------
; OK, we're done. Lets see what happens.

(install-mi "hello" "world)

; ------- THE END -------
