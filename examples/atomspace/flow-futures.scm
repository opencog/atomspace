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
	(cog-inc-count! (List THING-A THING-B) 1.0)
	(cog-inc-count! (List (AnyNode "left wildcard") THING-B) 1.0)
	(cog-inc-count! (List THING-A (AnyNode "right wildcard")) 1.0)
	(cog-inc-count! (AnyNode "grand total") 1.0))

; Same as above, but works with strings. It counts how often a pair
; was "observed".
(define (observe STRING-A STRING-B)
	(incr-counts (Concept STRING-A) (Concept STRING-B)))

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

; Get the value
(define (get-computed-value THING-A THING-B)
	(cog-value (List THING-A THING-B) (Predicate "MI Key")))

(define (get-mi-stream STRING-A STRING-B)
	(get-computed-value (Concept STRING-A) (Concept STRING-B)))

; -------------------------------------------------------------
; OK, we're done. Lets see what happens.

(install-mi "hello" "world")
(get-mi-stream "hello" "world")

; Well, that's messy! But it does show the full structure of
; what was set up. Note that the formulas were applied to the
; full vector of floats, which have the form (1 0 N) for some
; count N. Dividing by zero and taking a log will result in a
; NAN (Not A Number), and so the first two numbers are garbage.
; We only want the third number.
;
; Instead of treating the full vector, we could have written
; the formula to extract the third number at the beginning, and
; then only work with that. The extraction can be done with the
; DecimateLink. But that would make the demo too complicated, so
; that is not done. See however, below.

; We really ony want the third number. So grab that.
(define (get-mi STRING-A STRING-B)
	(cog-value-ref (get-mi-stream STRING-A STRING-B) 2))

(get-mi "hello" "world")

; And now play with the data. Observe the pair a second time.
; The MI will change.
(observe "hello" "world")
(get-mi "hello" "world")

; Observing other pairs will also change the MI
(observe "hello" "Gary")
(get-mi "hello" "world")

(observe "whats" "up")
(get-mi "hello" "world")

; Dump the contents of the AtomSpace. Review to make sure the
; demo was understood.
(cog-prt-atomspace)

; -------------------------------------------------------------
; Repeate some of the above, this time using the DecimateLink
; to pick out the desired component.

; Defina a bit-mask and install it.
(cog-set-value! (Concept "someplace") (Predicate "mask key")
	(BoolValue 0 0 1))

; Verify that masking works
(cog-execute!
	(Decimate
		(BoolValueOf (Concept "someplace") (Predicate "mask key"))
		(FloatValueOf (AnyNode "grand total") tvp)))

; Wrap it in a utility constructor
(define (make-deci ATOM)
	(Decimate
		(BoolValueOf (Concept "someplace") (Predicate "mask key"))
		(FloatValueOf ATOM tvp)))

; A new proceedure, that works only on the one item.
(DefineLink
	(DefinedProcedure "scalar MI")
	(Lambda
		(VariableList (Variable "$L") (Variable "$R"))
		(Log2
			(Divide
				(Times
					(make-deci (List (Variable "$L") (Variable "$R")))
					(make-deci (AnyNode "grand total")))
				(Times
					(make-deci (List (Variable "$L") (Any "right wildcard")))
					(make-deci (List (Any "left wildcard") (Variable "$R"))))))))

(define (install-scalar THING-A THING-B)
	(define pair (List THING-A THING-B))
	(cog-set-value! pair (Predicate "Alt MI Key")
		(FormulaStream
			(ExecutionOutput (DefinedProcedure "scalar MI") pair))))

; Convenience wrapper, works with strings.
(define (install-scalar-mi STRING-A STRING-B)
	(install-scalar (Concept STRING-A) (Concept STRING-B)))

; Get the value
(define (get-computed-scalar THING-A THING-B)
	(cog-value (List THING-A THING-B) (Predicate "Alt MI Key")))

(define (get-mi-scalar STRING-A STRING-B)
	(get-computed-scalar (Concept STRING-A) (Concept STRING-B)))

; Try it out
(get-mi-scalar "hello" "world")
(get-mi "hello" "world")

(observe "Hey" "Joe")

(get-mi-scalar "hello" "world")
(get-mi "hello" "world")

; ------- THE END -------
