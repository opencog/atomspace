;
; parallel.scm -- Multi-threading in Atomese
;
; Running programs and loops and what-not in Atomese can be awkward
; if you can't run things in parallel. Ah hah! But you can! The
; ParallelLink and the JoinLink allow threads to be created and joined.
;

(use-modules (opencog) (opencog exec))

(define nnn 0)
(define (incr) (set! nnn (+ nnn 1)) (stv 1 0))

(define pllel
	(Parallel
		(SequentialAnd
			(True (Sleep (Number 1)))
			(EvaluationLink
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 3)))
			(EvaluationLink
				(GroundedPredicate "scm:incr") (List)))
		(SequentialAnd
			(True (Sleep (Number 5)))
			(EvaluationLink
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
