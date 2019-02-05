;
; fsm-full.scm -- Finite State Machine (FSM) Demo.
;
; Based on `fsm-basic.scm`, this defines a very simple four-state finite
; state machine, but illustrates the general (universal) FSM state
; machine constructor.  This allows multiple FSM's to be simultaneously
; defined and operated asynchronously from each-other.
;
(use-modules (opencog))

;; Set of possible states of the state machine
;; This definition of the set of states is not strictly needed; it is
;; not used anywhere in the demo below.
(SetLink
	(ConceptNode "initial state")
	(ConceptNode "green")
	(ConceptNode "yellow")
	(ConceptNode "red")
)

(define my-trans (Concept "My FSM's Transition Rule"))
(define my-state (Anchor  "My FSM's Current State"))

;; The initial state of the FSM
(List
	my-state
	(Concept "initial state")
)

;; The set of allowed state transitions.  Its a triangular cycle,
;; of green going to yellow going to red going back to green.
;; The initial state transitions into green (and is never visited again).
;;
;; Each rule is labelled with the "my-trans", so that rules for
;; different FSM's do not clash with one-another.  A ContextLink is used
;; because that will allow this example to generalize: Context's are
;; usually used to  express conditional probabilities, so that 
;;
;;     Context  <TV>
;;         A
;;         B
;;
;; represents the probability of B conditioned on A, and the TV holds
;; the numeric value for P(B|A).  In this case, A is the current state
;; of the machine, and B the the next state of the machine, so that P(B|A)
;; is the probability of transitioning to state B give that the machine is
;; in state A.  Such a system is called a Markov Chain.
;; 
;; For the example below, P(B|A) is always one.

(ContextLink
	(Concept "initial state")
	(List
		my-trans
		(Concept "green")
	)
)

(ContextLink
	(Concept "green")
	(List
		my-trans
		(Concept "yellow")
	)
)

(ContextLink
	(Concept "yellow")
	(List
		my-trans
		(Concept "red")
	)
)

(ContextLink
	(Concept "red")
	(List
		my-trans
		(Concept "green")
	)
)

;;; A Universal Deterministic Finite State Machine Constructor.
;;;
;;; This will create a deterministic FSM; that is, a rule that will
;;; transition any arbitrary deterministic FSM from state to state,
;;; given only its name, and the name given to the transition rules.
;;;
;;; Create a BindLink that can take an FSM with the name `fsm-name`
;;; and stores it's state in `fsm-state`.  After the BindLink is
;;; created, each invocation of it will advance the FSM but one step.
;;;
(define (create-fsm fsm-name fsm-state)
	(Bind
		;; We will need to find the current and the next state
		(VariableList
			(Variable "$curr-state")
			(Variable "$next-state")
		)
		(And
			;; If we are in the current state ...
			(List
				fsm-state
				(Variable "$curr-state")
			)
			;; ... and there is a transition to another state...
			(Context
				(Variable "$curr-state")
				(List
					fsm-name
					(Variable "$next-state")
				)
			)
		)
		(And
			;; ... then transition to the next state ...
			(List
				fsm-state
				(Variable "$next-state")
			)
			;; ... and leave the current state.
			(Delete
				(List
					fsm-state
					(Variable "$curr-state")
				)
			)
		)
	)
)

;;; Create "my-fsm"
(define my-fsm (create-fsm my-trans my-state))

;;; Take one step.
;(cog-execute! my-fsm)

;;; Take three steps.
;;; Try it!
;(cog-execute! my-fsm)
;(cog-execute! my-fsm)
;(cog-execute! my-fsm)
