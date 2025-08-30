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
; Define a printer, to monitor the recursive call.

; This includes a sleep call, to slow things down.
; The reason for this will become apparent, shortly.
(define (my-print-func arg)
	(format #t "hi there! ~A\n" (cog-execute! arg))
	(sleep 1)
	(VoidValue))

(define printer
	(ExecutionOutput
		(GroundedSchema "scm:my-print-func")
		(List
			(ValueOf (Anchor "some place") (Predicate "number key")))))

; Run the printer, and verify that it works.
(cog-execute! printer)

(Define
	(DefinedProcedure "simple-tail")
	(PureExec (cog-atomspace)
		increment
		printer
		(DefinedProcedure "simple-tail")))

; This should print six times or so, maybe less, maybe more.
(cog-execute! (DefinedProcedure "simple-tail"))
