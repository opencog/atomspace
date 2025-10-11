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
			#t)
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
(cog-execute! pllel)

; And again, just for good luck!
(cog-execute! pllel)

; This is almost identical to the above, except that the
; ExecuteThreadedLink places the results of execution onto a
; QueueValue. The QueueValue is a thread-safe queue that is
; created open-for-writing by the writer, and remains open
; until the writer is done. Readers can read elements off the
; queue, one by one, or in a batch. If the queue is empty,
; readers will block until the writer places something on
; the queue, or until the writer closes the queue. When the
; queue is closed, readers will unblock (and get whatever is
; left, i.e. whatever they haven't dequeued already.)
;
; The `cog-value->list` tried to get the entire queue contents,
; in one big gulp. It blocks until the queue closes. Thus,
; running the below will block for five seconds.
;
(define wait
	(ExecuteThreaded
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
(cog-value->list (cog-execute! wait))
(cog-value->list (cog-execute! wait))
(cog-value->list (cog-execute! wait))
