;
; tail-procedure.scm - Writing general recursive loops.
;
; This explores several variants of loops that can be built from
; `DefinedProcedure`.
;
(use-modules (opencog) (opencog exec))

; Place a FloatValue at a location where it can be found.
(cog-set-value!
	(Anchor "some place")
	(Predicate "number key")
	(FloatValue 0 0 0))

; The current value at that location is obtained by executing the
; ValueOfLink. Do that now.
(cog-execute! (ValueOf (Anchor "some place") (Predicate "number key")))

; Declare an Atomese expression that fetches the numeric value at
; that location, increments it, and stores it back.
(define increment
	(SetValue (Anchor "some place") (Predicate "number key")
		(PlusLink
			(NumberNode 1 2 3)
			(FloatValueOf (Anchor "some place") (Predicate "number key")))))

; Perform the increment by executing the declared expression:
(cog-execute! increment)

; Take a look, and see that the Value has been updated:
(cog-execute! (ValueOf (Anchor "some place") (Predicate "number key")))

; Do it a few more times, to be sure.
(cog-execute! increment)
(cog-execute! increment)
(cog-execute! increment)

;; -------------------------------------

(Define
	(DefinedProcedure "simple-tail")
	(PureExec (cog-atomspace)
		increment
		(print)
		(DefinedProcedure "simple-tail")))

; Print something.
(define (print-stuff) (display "hi there!\n") (stv 1 1))

      (Evaluation (GroundedPredicate "scm: print-stuff") (List))

; This should print six times or so, maybe less, maybe more.
(cog-execute! (DefinedProcedure "simple-tail"))
