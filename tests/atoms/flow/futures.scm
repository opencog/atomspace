;
; futures.scm -- unit test corresponding to the demo flow-futures.scm
;
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

(install-mi "hello" "world")

; -------------------------------------------------------------

; Defina a bit-mask and install it.
(cog-set-value! (Concept "someplace") (Predicate "mask key")
	(BoolValue 0 0 1))

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

; Try it out. Need to install it before first use.
(install-scalar-mi "hello" "world")

; ------- THE END -------
