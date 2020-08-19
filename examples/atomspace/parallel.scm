;
; parallel.scm -- Multi-threading in Atomese
;
; Running programs and loops and what-not in Atomese can be awkward
; if you can't run things in parallel. Ah hah! But you can! The
; ParallelLink and the JoinThreadLink allow threads to be created and
; joined. This example shows how.
;
; See also `threaded.scm` for the ExecuteThreadedLink, which might be
; easier to use in many situations.
;

(use-modules (opencog) (opencog exec))

; Define a simple counter that will be incremented
(define (make-counter)
	(let ((nnn 1))
		(lambda ()
			(format #t "Finished running the thread; exiting!\n")
			(format #t " -- This is the ~A'th we've finished!\n" nnn)
			(format #t " -- The time is ~A\n\n"
				(strftime "%c" (localtime (current-time))))
			(set! nnn (+ nnn 1)) ; This increment is not thread-safe!
			(stv 1 0))
	))

(define incr (make-counter))

; Define three detached threads. When this is evaluated, the
; ParallelLink will create three detached threads, one for each
; Atom in it's outgoing set. (One thread for each SequentialAndLink).
; It will then return immediately to the caller.  Meanwhile, each thread
; will go off and do it's thing. The three threads will each sleep
; a varying amount, and print a message when they are done.
(define pllel
	(Parallel
		(SequentialAnd
			(True (Sleep (Number 1)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 3)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 5)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
	))

; So create the thread, and run it.
(cog-evaluate! pllel)

; And again, just for good luck!
(cog-evaluate! pllel)

; This is almost identical to the above, except that the ThreadJoinLink
; will not return control to the evaluator until all of the threads
; have finished. The longest running thread takes five seconds, so
; sit back and relax and watch the pretty messages appear.
(define wait
	(ThreadJoin
		(SequentialAnd
			(True (Sleep (Number 1)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 3)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 5)))
			(Evaluation
				(GroundedPredicate "scm:incr") (List)))
	))

; Wait for the threads to finish.
(cog-evaluate! wait)
(cog-evaluate! wait)
(cog-evaluate! wait)
