;
; parallel.scm -- Multi-threading in Atomese
;
; Running programs and loops and what-not in Atomese can be awkward
; if you can't run things in parallel. Ah hah! But you can! The
; ParallelLink and the JoinLink allow threads to be created and joined.
;

(use-modules (opencog) (opencog exec))

; Define a simple counter that will be incremented
(define (make-counter)
	(let ((nnn 1))
		(lambda ()
			(format #t "Entering the counter for the ~A'th time!\n" nnn)
			(format #t " -- The time is ~A\n"
				(strftime "%c" (localtime (current-time))))
			(set! nnn (+ nnn 1))
			(stv 1 0))
	))

(define incr (make-counter))

; Define a dettached thread
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

(define wait
	(Join
		(SequentialAnd
			(True (Sleep (Number 1)))
			(EvaluationLink
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(False (Sleep (Number 3)))
			(EvaluationLink
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 5)))
			(EvaluationLink
				(GroundedPredicate "scm:incr") (List)))
	))
