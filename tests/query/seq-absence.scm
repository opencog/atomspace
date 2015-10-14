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
			(PresentLink (EvaluationLink (PredicateNode "or-visible")
					(ListLink (VariableNode "$x"))))
			;; If above fails then set state
			(TrueLink (PutLink
					(StateLink (AnchorNode "state") (VariableNode "$yy"))
					(ConceptNode "not-vis")))
		)))

(define or-visible-put
	(SatisfactionLink
		;; SequentialOrLink - verify predicates in sequential order.
		(SequentialOrLink
			(PresentLink (EvaluationLink (PredicateNode "yes-visible")
					(ListLink (VariableNode "$x"))))
			;; If above fails then set state
			(TrueLink (PutLink
					(StateLink (AnchorNode "state") (VariableNode "$yy"))
					(ConceptNode "ohhh noot visible")))
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

;; You might think that this one is similar to the above, but the
;; Pattern matcher treates present and Absent links in a very
;; different way. This one actually does not work correctly,
;; and right now, I'm not gonna fix it... XXX FIXME.
(define and-not-present
	(SatisfactionLink
		(SequentialAndLink
			(NotLink (PresentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x")))))
			;; If above fails then increment
			(EvaluationLink
				(GroundedPredicateNode "scm: incr-trig") (ListLink))
		)))

;; This one is a bit perverted, and similar expressions are guaranteed
;; to fail in the general case, cause teh pattern matcher treats
;; AbsentLinks in a fundamentally different way. But we test this
;; anyway, for now.  Like the above, its currently broken. Its too
;; weird right now for me to want to fix it, so I am punting on this.
;; XXX FIXME ... this and the above need to get done right.

(define or-not-absent
	(SatisfactionLink
		(SequentialOrLink
			(NotLink (AbsentLink (EvaluationLink (PredicateNode "visible")
					(ListLink (VariableNode "$x")))))
			;; If above fails then increment
			(EvaluationLink
				(GroundedPredicateNode "scm: incr-trig") (ListLink))
		)))

; ------------------------------------------------------
