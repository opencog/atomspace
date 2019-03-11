;
; fsm-basic.scm -- Very Simple Finite State Machine (FSM) Demo.
;
; This defines a very simple four-state finite state machine: an initial
; state, and a cycle of three state: green, yellow, red. It then uses a
; BindLink to take one step at a time.  The BindLink is more or less
; "universal", in that it can run any FSM, not  just this one. However,
; here it has been a bit simplified, to keep the demo as simple as
; possible. See the file `fsm-full.scm` for the correct way to define a
; truly general-purpose FSM, in such a way that multiple FSM's can be run
; at the same time.
;
(use-modules (opencog))

;; Set of possible states of the state machine.
;; The definition of this set is not strictly needed; it is not used
;; for anything in the demo code below.
(Set
	(Concept "initial state")
	(Concept "green")
	(Concept "yellow")
	(Concept "red")
)

;; The initial state of the FSM
(List
	(Anchor "Current State")
	(Concept "initial state")
)

;; The set of allowed state transitions.  Its a triangular cycle,
;; of green going to yellow going to red going back to green.
;; The initial state transitions into green (and is never visited again).
(List
	(Concept "initial state")
	(Concept "green")
)

(List
	(Concept "green")
	(Concept "yellow")
)

(List
	(Concept "yellow")
	(Concept "red")
)

(List
	(Concept "red")
	(Concept "green")
)


;;; Take one step of a finite state machine.
;;; Note that the algorithm below is "universal": it can run any FSM,
;;; it does not care about the specific states or state transition
;;; rules.
;;;
;;; To define two or more machines that run at the same time, each
;;; should use a different AnchorNode, so that the states of the
;;; various machines would not get confused with one-another.
;;; It would also probably be a good idea to include the machine name
;;; (i.e. the AnchorNode) as part of the transition rules, so that
;;; the transition rules for one machine would not get used accidentally
;;; for another machine.
(define take-one-step
	(Bind
		;; We will need to find the current and the next state
		(VariableList
			(Variable "$curr-state")
			(Variable "$next-state")
		)
		(And
			;; If we are in the current state ...
			(List
				(Anchor "Current State")
				(Variable "$curr-state")
			)
			;; ... and there is a transition to another state...
			(List
				(Variable "$curr-state")
				(Variable "$next-state")
			)
		)
		(And
			;; ... then transition to the next state ...
			(List
				(Anchor "Current State")
				(Variable "$next-state")
			)
			;; ... and leave the current state.
			(Delete
				(List
					(Anchor "Current State")
					(Variable "$curr-state")
				)
			)
		)
	)
)

;; Take on step of the FSM
;(cog-execute! take-one-step)
;
;;; Take three more steps;
;;; Try it!  See what happens!
;(cog-execute! take-one-step)
;(cog-execute! take-one-step)
;(cog-execute! take-one-step)
