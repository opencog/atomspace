;;
;; seq-absence.scm
;;
;; Test the AbsentLink inside a SequentialAnLink.
;;
;; Check to see if the atomspace does NOT the link
;;    (EvaluationLink
;;        (PredicateNode "visible")
;;            (ListLink
;;               (ConceptNode "anything")))
;;

; ------------------------------------------------------

(define or-put
	(SatisfactionLink
		;; SequentialOrLink - verify predicates in sequential order.
		(SequentialOrLink
			(PresentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x"))))
			;; If above fails then set state
			(TrueLink (PutLink
					(StateLink (AnchorNode "state") (VariableNode "$yy"))
					(ConceptNode "not-vis")))
		)))

(define trig 0)
(define (incr-trig) (set! trig (+ trig 1)) (stv 1 1))

(define or-presence
	(SatisfactionLink
		;; SequentialOrLink - verify predicates in sequential order.
		(SequentialOrLink
			(PresentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x"))))
			;; If above fails then increment
			(EvaluationLink
				(GroundedPredicateNode "scm: incr-trig") (ListLink))
		)))

(define and-absence
	(SatisfactionLink
		(SequentialAndLink
			(AbsentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x"))))
			;; If above fails then increment
			(EvaluationLink
				(GroundedPredicateNode "scm: incr-trig") (ListLink))
		)))

; ------------------------------------------------------
