
(use-modules (opencog) (opencog exec))

(define nnn 0)
(define (incr) (set! nnn (+ nnn 1)) (stv 1 0))

(define pllel
	(Parallel
		(SequentialAnd
			(TrueLink (Sleep (Number 1)))
			(EvaluationLink
				(GroundedPredicateNode "scm:incr") (List)))
		(SequentialAnd
			(TrueLink (Sleep (Number 3)))
			(EvaluationLink
				(GroundedPredicateNode "scm:incr") (List)))
		(SequentialAnd
			(TrueLink (Sleep (Number 5)))
			(EvaluationLink
				(GroundedPredicateNode "scm:incr") (List)))
	))
