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
	(format #t "hi there! ~A" (cog-execute! arg))
	(sleep 1)
	(VoidValue))

(define print-stuff
	(ExecutionOutput
		(GroundedSchema "scm:my-print-func")
		(List
			(ValueOf (Anchor "some place") (Predicate "number key")))))

; Run the printer, and verify that it works.
(cog-execute! print-stuff)

; Define an infinite loop tail call. This will run forever, or, at
; least, until you ctrl-C the terminal.
(Define
	(DefinedProcedure "simple-tail")
	(PureExec (cog-atomspace)
		increment
		print-stuff
		(DefinedProcedure "simple-tail")))

; Try it! This will run forever, until you kill it.
(cog-execute! (DefinedProcedure "simple-tail"))

; -----------------------------------------------------------
; The loop can be limited to run for a shorter time. In this example,
; a random stopping condition is added.

; Create a stream of random-number values, between 0 and 1.
; See `stream.scm` for an extended demonstration of streams.
(cog-set-value!
	(Anchor "some place")
	(Predicate "randgen")
	(RandomStream 1))

; Define a predicate that tests to see if a random number is
; less than 0.9. If it is, true is returned; else false.
(Define (DefinedPredicate "keep going?")
   (GreaterThan (Number 0.9)
		(FloatValueOfLink (Anchor "some place") (Predicate "randgen"))))

; Define another debug printer
(define (print-done)
	(display "We are done now!\n")
	(VoidValue))

; Use a CondLink to determine whether to keep looping, or not.
(Define
	(DefinedProcedure "stop-randomly")
	(PureExec (cog-atomspace)
		increment
		print-stuff
		(CondLink   ; if-then-else
			(DefinedPredicate "keep going?")
			(DefinedProcedure "stop-randomly")
			(ExecutionOutput (GroundedSchema "scm:print-done") (List)))))

; This will typically print about 3 or 4 times, but sometimes much
; longer.
(cog-execute! (DefinedProcedure "stop-randomly"))

; Try it again.
(cog-execute! (DefinedProcedure "stop-randomly"))
(cog-execute! (DefinedProcedure "stop-randomly"))
(cog-execute! (DefinedProcedure "stop-randomly"))

; -----------------------------------------------------------
; Calls can be chained. In this demo, procedure A calls B calls C,
; which calls A again. This continues, until the (random) termination
; condition is met.
;
; One notable aspect of this demo is that DefinedProcedure is
; implemented in such a way as to not blow out the C++ stack,
; even when doing long nested recursive loops such as this.
; In the following, the C++ stack will grow for A, B and C, but
; when A is called again, there will be a return (in the C++ code)
; from C to B and back to A, whereupon the procedure for A is
; run again. This can continue forever, without risk of blowing up
; the C++ stack.

(define (print-A) (display "I am procedure A!\n") (VoidValue))
(define (print-B) (display "I am procedure B!\n") (VoidValue))
(define (print-C) (display "I am procedure C!\n") (VoidValue))

(Define
	(DefinedProcedure "procedure A")
	(PureExec (cog-atomspace)
		increment
		(ExecutionOutput (GroundedSchema "scm:print-A") (List))
		(DefinedProcedure "procedure B")))

(Define
	(DefinedProcedure "procedure B")
	(PureExec (cog-atomspace)
		increment
		(ExecutionOutput (GroundedSchema "scm:print-B") (List))
		(DefinedProcedure "procedure C")))

(Define
	(DefinedProcedure "procedure C")
	(PureExec (cog-atomspace)
		increment
		(ExecutionOutput (GroundedSchema "scm:print-C") (List))
		(CondLink
			(DefinedPredicate "keep going?")
			(DefinedProcedure "procedure A")
			(ExecutionOutput (GroundedSchema "scm:print-done") (List)))))

; Try it!
(cog-execute! (DefinedProcedure "procedure A"))

; Try it again! It will randomly run for longer or shorter, each time.
(cog-execute! (DefinedProcedure "procedure A"))
(cog-execute! (DefinedProcedure "procedure A"))
(cog-execute! (DefinedProcedure "procedure A"))
(cog-execute! (DefinedProcedure "procedure A"))

; ----------------- That's all, Folks! The End! -----------------
