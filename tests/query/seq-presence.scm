;;
;; seq-presence.scm
;;
;; Test the PresentLink inside a SequentialAndLink.
;;
;; Check to see if the atomspace contains the link
;;    (ListLink (AnchorNode "Room State") (ConceptNode "room empty"))
;;
;; If the atomspace does contain the above, then the print-msg function
;; is run.

(use-modules (opencog) (opencog exec))

;; Is the room empty, or is someone in it?
;; One of several different states can be linked to the state variable.
(define room-state (AnchorNode "Room State"))
(define room-empty (ConceptNode "room empty"))
(define room-nonempty (ConceptNode "room nonempty"))

;; Assume room empty at first
(ListLink room-state room-empty)

; Print an atom, return a TV value
(define (tv-print-atom atom)
	(format #t "Hello, I got this atom: ~a\n" atom) (stv 1 1))

; ------------------------------------------------------
; ------------------------------------------------------

;; This variant uses PresentLink to place the room-state into
;; a variable, and then uses EqualLink to check the value of
;; that variable.
;;
;; This variant haas an advantage over the next one, as it requires
;; only one invocation of the pattern matcher, not two. Because it
;; uses the SequentialAndLink, it is in a form appropriate for creating
;; a behavior tree.
;;
(define empty-sequence
	(SatisfactionLink
		;; SequentialAndLink - verify predicates in sequential order.
		(SequentialAndLink
			;; Assign the room-state to variable $x
			(PresentLink (ListLink room-state (VariableNode "$x")))
			;; If the variable $x equals the emtpry state, then ...
			(EqualLink (VariableNode "$x") room-empty)
			;; ... then print a message.
			(EvaluationLink
				(GroundedPredicateNode "scm: tv-print-atom")
				(ListLink (VariableNode "$x")))
		)))

; (cog-evaluate! empty-sequence)

; ------------------------------------------------------
