;
; Mealy Finite State Machine (FSM) Demo.
;
; Based on fsm-full.scm, this alters the general FSM defintion to
; include a dependency on the external state.
;
(use-modules (opencog))
(use-modules (opencog query))

(define my-trans (ConceptNode "My FSM's Transition Rule"))
(define my-state (AnchorNode "My FSM's Current State"))

(define halt-state (ConceptNode "halt state"))
(define red-state (ConceptNode "red state"))
(define green-state (ConceptNode "green state"))
(define blue-state (ConceptNode "blue state"))
(define cyan-state (ConceptNode "cyan state"))
(define magenta-state (ConceptNode "magenta state"))
(define yellow-state (ConceptNode "yellow state"))

; External state will control the behavior of the FSM.
(define extern-anchor (PredicateNode "External State"))

; External, environmental commands.
(define go-forward (ConceptNode "forward"))
(define go-reverse (ConceptNode "reverse"))
(define halt (ConceptNode "halt"))

;; The inital state of the FSM
(ListLink my-state halt-state)

;; The set of allowed state transistions.  The transitions depend on
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
(ContextLink (AndLink halt-state halt) (ListLink my-trans halt-state))
(ContextLink (AndLink red-state halt) (ListLink my-trans halt-state))
(ContextLink (AndLink green-state halt) (ListLink my-trans halt-state))
(ContextLink (AndLink blue-state halt) (ListLink my-trans halt-state))
(ContextLink (AndLink cyan-state halt) (ListLink my-trans halt-state))
(ContextLink (AndLink magenta-state halt) (ListLink my-trans halt-state))
(ContextLink (AndLink yellow-state halt) (ListLink my-trans halt-state))

; The forward cycle
(ContextLink (AndLink halt-state go-foreward) (ListLink my-trans red-state))
(ContextLink (AndLink red-state go-foreward) (ListLink my-trans green-state))
(ContextLink (AndLink green-state go-foreward) (ListLink my-trans blue-state))
(ContextLink (AndLink blue-state go-foreward) (ListLink my-trans red-state))

; A reversed state halts before moving forward
(ContextLink (AndLink cyan-state go-forward) (ListLink my-trans halt-state))
(ContextLink (AndLink magenta-state go-forward) (ListLink my-trans halt-state))
(ContextLink (AndLink yellow-state go-forward) (ListLink my-trans halt-state))

; The reverse cycle
(ContextLink (AndLink halt-state go-reverse) (ListLink my-trans cyan-state))
(ContextLink (AndLink cyan-state go-reverse) (ListLink my-trans magenta-state))
(ContextLink (AndLink magenta-state go-reverse) (ListLink my-trans yellow-state))
(ContextLink (AndLink yellow-state go-reverse) (ListLink my-trans cyan-state))

; Stop before reversing.
(ContextLink (AndLink red-state go-reverse) (ListLink my-trans halt-state))
(ContextLink (AndLink green-state go-reverse) (ListLink my-trans halt-state))
(ContextLink (AndLink blue-state go-reverse) (ListLink my-trans halt-state))

;;; A Universal Deterministic Finite State Machine Constructor.
;;;
;;; This will create a deterministic FSM; that is, a rule that will
;;; transition any arbitrary deterministic FSM from state to state,
;;; given only its name, and the name given to the transition rules.
;;;
;;; Create a BindLink that can take an FSM with the name `fsm-name`
;;; and stores it's state in `fsm-state`.  After the BindLink is
;;; created, each invocation of it will advance the FSM bu one step.
;;;
(define (create-fsm fsm-name fsm-state extern-state)
	(BindLink
		;; We will need to find the current and the next state
		(VariableList
			(VariableNode "$extern-state")
			(VariableNode "$curr-state")
			(VariableNode "$next-state")
		)
		(ImplicationLink
			(AndLink
				;; If we are in the current state ...
				(ListLink
					fsm-state
					(VariableNode "$curr-state")
				)
				;; ... and the external environment is in the given state
				(EvaluationLink
					extern-state
					(VariableNode "$extern-state")
				)
				;; ... and there is a transition to another state...
				(ContextLink
					(AndLink
						(VariableNode "$curr-state")
						(VariableNode "$extern-state")
					)
					(ListLink
						fsm-name
						(VariableNode "$next-state")
					)
				)
			)
			(AndLink
				;; ... then transistion to the next state ...
				(ListLink
					fsm-state
					(VariableNode "$next-state")
				)
				;; ... and leave the current state.
				(DeleteLink
					(ListLink
						fsm-state
						(VariableNode "$curr-state")
					)
				)
			)
		)
	)
)

;;; Create "my-fsm"
(define my-fsm (create-fsm my-trans my-state extern-anchor))

;;; Take one step.
;(cog-bind my-fsm)

;;; Take three steps.
;;; Try it!
;(cog-bind my-fsm)
;(cog-bind my-fsm)
;(cog-bind my-fsm)
