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

(define or-sequence
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

; ------------------------------------------------------
