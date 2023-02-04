;
; fsm-mealy.scm -- Mealy Finite State Machine (FSM) Demo.
;
; Based on fsm-full.scm, this alters the general FSM definition to
; include a dependency on the external state.
;
; To run this demo, load this file:
; (add-to-load-path ".")
; (load-from-path "fsm-mealy.scm")
;
; Then, scroll to the bottom, and try some of the commented-out examples.

(use-modules (opencog)(opencog exec))

(define my-trans (Concept "My FSM's Transition Rule"))
(define my-state (Anchor  "My FSM's Current State"))

(define halt-state (Concept "halt state"))
(define red-state (Concept "red state"))
(define green-state (Concept "green state"))
(define blue-state (Concept "blue state"))
(define cyan-state (Concept "cyan state"))
(define magenta-state (Concept "magenta state"))
(define yellow-state (Concept "yellow state"))

; External state will control the behavior of the FSM.
(define extern-anchor (Predicate "External State"))

; External, environmental commands.
(define go-forward (Concept "forward"))
(define go-reverse (Concept "reverse"))
(define halt (Concept "halt"))

;; The initial state of the FSM
(List my-state halt-state)

;; The current environment
(Evaluation extern-anchor halt)

;; The set of allowed state transitions.  The transitions depend on
;; both the current state, and the external state; thus, this is
;; effectively a Mealy machine.
;;
;; Three cycles are implemented: a unit-length no-op cycle, a "forward"
;; cycle going through red-green-blue, and a "reverse" cycle going
;; through cyan-magenta-yellow.
;;
;; Each rule is labelled with the "my-trans", so that rules for
;;

; All states transition to halt upon halt.
(ContextLink (And halt-state halt)  (List my-trans halt-state))
(ContextLink (And red-state halt)   (List my-trans halt-state))
(ContextLink (And green-state halt) (List my-trans halt-state))
(ContextLink (And blue-state halt)  (List my-trans halt-state))
(ContextLink (And cyan-state halt)  (List my-trans halt-state))
(ContextLink (And magenta-state halt) (List my-trans halt-state))
(ContextLink (And yellow-state halt) (List my-trans halt-state))

; The forward cycle
(ContextLink (And halt-state go-forward)  (List my-trans red-state))
(ContextLink (And red-state go-forward)   (List my-trans green-state))
(ContextLink (And green-state go-forward) (List my-trans blue-state))
(ContextLink (And blue-state go-forward)  (List my-trans red-state))

; A reversed state halts before moving forward
(ContextLink (And cyan-state go-forward)    (List my-trans halt-state))
(ContextLink (And magenta-state go-forward) (List my-trans halt-state))
(ContextLink (And yellow-state go-forward)  (List my-trans halt-state))

; The reverse cycle
(ContextLink (And halt-state go-reverse) (List my-trans cyan-state))
(ContextLink (And cyan-state go-reverse) (List my-trans magenta-state))
(ContextLink (And magenta-state go-reverse) (List my-trans yellow-state))
(ContextLink (And yellow-state go-reverse) (List my-trans cyan-state))

; Stop before reversing.
(ContextLink (And red-state go-reverse) (List my-trans halt-state))
(ContextLink (And green-state go-reverse) (List my-trans halt-state))
(ContextLink (And blue-state go-reverse) (List my-trans halt-state))

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
(define (create-fsm fsm-name fsm-state extern-state)
	(BindLink
		;; We will need to find the current and the next state
		(VariableList
			(Variable "$extern-state")
			(Variable "$curr-state")
			(Variable "$next-state")
		)
		(And
			;; If we are in the current state ...
			(List
				fsm-state
				(Variable "$curr-state")
			)
			;; ... and the external environment is in the given state
			(EvaluationLink
				extern-state
				(Variable "$extern-state")
			)
			;; ... and there is a transition to another state...
			(ContextLink
				(And
					(Variable "$curr-state")
					(Variable "$extern-state")
				)
				(List
					fsm-name
					(Variable "$next-state")
				)
			)
		)
		(And
			;; ... Then, leave the current state ...
			(DeleteLink
				(List
					fsm-state
					(Variable "$curr-state")
				)
			)
			;; ... and transition to the next state.
			(List
				fsm-state
				(Variable "$next-state")
			)
		)
	)
)

;;; Create "my-fsm"
(define my-fsm (create-fsm my-trans my-state extern-anchor))

;;; A utility to take a step, and display the new state
(define (take-step) (gar (gar (cog-execute! my-fsm))))

;;; A utility to show the current FSM state
(define (show-fsm-state)
	(car (cog-chase-link 'ListLink 'ConceptNode my-state)))

;;; As above, but show the environment
(define (show-environment-state)
	(car (cog-chase-link 'EvaluationLink 'ConceptNode extern-anchor)))

; Set the direction
(define (move-dir dir)
	; First, delete the current external state
	(cog-extract! (Evaluation extern-anchor (show-environment-state)))
	; Next, set the new direction
	(Evaluation extern-anchor dir))

; Set the direction
(define (move-forward) (move-dir go-forward))
(define (move-reverse) (move-dir go-reverse))
(define (move-halt) (move-dir halt))

;;; Take one step.
;(take-step)

; View the current state:
(show-fsm-state)

;;; Take three steps.
;;; Try it!
; (take-step)
; (take-step)
; (take-step)
; (move-forward)
; (take-step)
; (take-step)
; (take-step)
; (move-reverse)
; (take-step)
; (take-step)
; (take-step)
; (move-halt)
