;
; markov-chain.scm -- Probabilistic Finite State Machine (Markov Chain).
;
; Based on `fsm-full.scm`, this defines a very simple four-state Markov
; chain, using the same states as the demo FSM's. The difference here is
; that the transitions are specified probabilistically; multiple
; transitions may occur; each transition has a fixed probability.
;
; Another way to look at this is an example of matrix multiplication.
; The state of a Markov chain is a vector, and the state transitions
; can be understood as a matrix (a Markov matrix, where the columns
; sum to one).  One iteration of the probabilistic state machine
; corresponds to one matrix multiplication.
;
; In either viewpoint, the numbers, which are probabilities, are
; stored in the "strength" portion of a SimpleTruthValue.
;
; The easiest way to run this is to load this file:
; (add-to-load-path ".")
; (load-from-path "markov-chain.scm")
;
; Then, scroll to the bottom, and some of the commented-out
; examples.

(use-modules (opencog) (opencog query))

;; Define three objects: a name for the current state vector,
;; a name for the next state vector, and a name for the state
;; transition matrix.
(define my-trans (Concept "My Chain's Transition Rule"))
(define my-state (Anchor "My Chain's Current State"))
(define my-nexts (Anchor "My Chain's Next State"))

;; The initial state of the Markov chain.  It starts with 100%
;; probability in this state.
( (stv 1 1)
	my-state
	(Concept "initial state")
)
( (stv 0 1)
	my-state
	(Concept "green")
)
( (stv 0 1)
	my-state
	(Concept "yellow")
)
( (stv 0 1)
	my-state
	(Concept "red")
)

;; --------------------------------------------------------------------
;; The set of allowed state transitions.  Its a triangular cycle,
;; of green going to yellow going to red going back to green.
;; The initial state transitions into green (and is never visited again).
;;
;; Each rule is labelled with the "my-fsm", so that rules for
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

; Transition from initial to green with 90% probability.
(ContextLink (stv 0.9 1)
	(Concept "initial state")
	(
		my-trans
		(Concept "green")
	)
)

; Transition from initial state to yellow with 10% probability.
(ContextLink (stv 0.1 1)
	(Concept "initial state")
	(
		my-trans
		(Concept "yellow")
	)
)

; Stay in the initial state with probability zero
(ContextLink (stv 0.0 1)
	(Concept "initial state")
	(
		my-trans
		(Concept "initial state")
	)
)

; Transition from green to yellow with 90% probability
(ContextLink (stv 0.9 1)
	(Concept "green")
	(
		my-trans
		(Concept "yellow")
	)
)

; Transition from green to red with 10% probability
(ContextLink (stv 0.1 1)
	(Concept "green")
	(
		my-trans
		(Concept "red")
	)
)

; Transition from yellow to red with 90% probability
(ContextLink (stv 0.9 1)
	(Concept "yellow")
	(
		my-trans
		(Concept "red")
	)
)

; Transition from yellow to green with 10% probability
(ContextLink (stv 0.1 1)
	(Concept "yellow")
	(
		my-trans
		(Concept "green")
	)
)

; Transition from red to green with 90% probability
(ContextLink (stv 0.9 1)
	(Concept "red")
	(
		my-trans
		(Concept "green")
	)
)

; Stay in the red state with 10% probability
(ContextLink (stv 0.1 1)
	(Concept "red")
	(
		my-trans
		(Concept "red")
	)
)

;; --------------------------------------------------------------------
;;; Create a BindLink that can take a Markov Chain with the name
;;; `chain-name` and two state vectors: `chain-state` and `chain-next`
;;; Each invocation of the BindLink will take the current state vector,
;;; multiply it by the transition matrix, and store the result in the
;;; `chain-next` vector.
;;;
(define (create-chain-stepper chain-name chain-next chain-state)
	(define curr-state
		(
			chain-state
			(Variable "$curr-state")
		)
	)
	(define state-trans
		(ContextLink
			(Variable "$curr-state")
			(
				chain-name
				(Variable "$next-state")
			)
		)
	)
	(define next-state
		(
			chain-next
			(Variable "$next-state")
		)
	)
	(Bind
		;; We will need to find the current and the next state
		(VariableList
			(Variable "$curr-state")
			(Variable "$next-state")
		)
		(And
			;; If we are in the current state ...
			curr-state
			;; ... and there is a transition to another state...
			state-trans
		)
		;; ... then adjust the probability...
		(ExecutionOutput
			(GroundedSchema "scm: accum-probability")
			(
				next-state
				state-trans
				curr-state
			)
		)
	)
)

;; --------------------------------------------------------------------
;; To move from state to state, we must have some way of accumulating
;; conditional probabilities.  This is done by the `accum-probability`
;; function, defined at the end of this section.  It accumulates
;; probability using the standard Bayesian interpretation.  All
;; the other functions here are helpers, to extract the probability
;; from a truth value, and to set it.

;;; Get the probability on atom. The probability is assumed to be stored
;;; in the "mean" component of a SimpleTruthValue.
(define (get-prob atom)
	(assoc-ref (cog-tv->alist (cog-tv atom)) 'mean))

;;; Return true if the TV on the atom is the default TV.
;;; For our purposes, we treat it as the default if the confidence is
;;; below 0.5 (since we later set confidence to 1.0)
(define (is-default-tv? atom)
	(not (< 0.5 (assoc-ref (cog-tv->alist (cog-tv atom)) 'confidence))))

;;; Set a probability value on the atom.
(define (set-prob atom value)
	(cog-set-tv! atom (cog-new-stv value 1.0)))

;;; Accumulate probability (add it to the existing value)
(define (accum-prob atom value)
	(if (is-default-tv? atom)
		(set-prob atom value)
		(set-prob atom (+ (get-prob atom) value))))

;;; Define a function to accumulate the probabilities
;;; It takes as input three atoms: PB, PBA and PA. The truth values on
;;; the last two correspond to P(B|A) (the probability of the state
;;; transition to B, given state A) and P(A) (the probability of being
;;; in state A).  The probability of B is then accumulated:
;;; P(B) += P(B|A) * P(A)
(define (accum-probability PB PBA PA)
	(accum-prob PB (* (get-prob PBA) (get-prob PA)))
	PB
)

;; --------------------------------------------------------------------
;;; Create a BindLink that will find a state vector, and delete it.
;;; After the next chain state is computed, it must be made into the
;;; current chain state.  This is done in a three-step process:
;;; 1) delete the current state vector
;;; 2) copy the next state vector to the current vector
;;; 3) delete the next state-vector.
;;; The below implements steps 1 and 3
(define (create-chain-deleter chain-state)
	(Bind
		(Variable "$state")
        ;; Find the state vector...
        (List chain-state (Variable "$state"))
        ;; Delete the state vector.
        (Delete
           (List chain-state (Variable "$state"))
		)
	)
)

;; --------------------------------------------------------------------
;; Copy a state vector from chain-from to chain-to
;; Since the TV's carry the probabilities, they must be copied.

;; Copy TV from second atom to first
(define (copy-tv b a)
	(begin (cog-set-tv! b (cog-tv a)) b))

(define (create-chain-copier chain-to chain-from)
	(Bind
		(Variable "$state")
        ;; Find the copy-from state vector...
        (List
            chain-from
            (Variable "$state")
        )
        ;; Copy it to the copy-to state vector.
        ;; We need to use an execution-output link to copy
        ;; the tv values from one to the other.
        (ExecutionOutput
            (GroundedSchema "scm:copy-tv")
            (List
                (List chain-to (Variable "$state"))
                (List chain-from (Variable "$state"))
			)
		)
	)
)

;; --------------------------------------------------------------------
;; Move a state vector from chain-from to chain-to
;; This combines the copy and delete operation into one.
;; It should be a bit faster.
(define (create-chain-move chain-to chain-from)
	(Bind
		; Constrain the allowed types on the variable;
		; we only want to copy the actual state, and not
		; (for example) the subgraphs of this link.
		(TypedVariable
			(Variable "$state")
			(Type "Concept")
		)
        ;; Find the copy-from state vector...
        (List
            chain-from
            (Variable "$state")
		)
        (And
            ;; Copy it to the copy-to state vector.
            ;; We need to use an execution-output link to copy
            ;; the tv values from one to the other.
            (ExecutionOutput
                (GroundedSchema "scm:copy-tv")
                (List
                    (List chain-to (Variable "$state"))
                    (List chain-from (Variable "$state"))
                )
            )
            ;; Delete the copy-from state vector
            (Delete
                (List chain-from (Variable "$state"))
			)
		)
	)
)

;; --------------------------------------------------------------------
;; Create a utility to show the state probabilities

(define (show-state state-vect)
	(define (get-tv atom) (cog-tv (List state-vect atom)))

	(format #t "State vector for ~A\n" (cog-name state-vect))

	(format #t "Initial state: ~A\n" (get-tv (Concept "initial state")))

	(format #t "Green state: ~A\n" (get-tv (Concept "green")))

	(format #t "Yellow state: ~A\n" (get-tv (Concept "yellow")))

	(format #t "Red state: ~A\n" (get-tv (Concept "red")))
)

;; --------------------------------------------------------------------
;;; Create a Markov chain stepper.  There are three parts to it.
;;; The first part multiplies the current state by the transition
;;; matrix, putting the result into the next-state vector.  The
;;; second part deletes the current-state vector. The third part
;;; moves the next-state vector to the current-state.
(define (take-a-step)
	(define my-stepper (create-chain-stepper my-trans my-nexts my-state))
	(define my-delter (create-chain-deleter my-state))
	(define my-mover (create-chain-move my-state my-nexts))
	(cog-execute! my-stepper)
	; (show-state my-nexts)
	(cog-execute! my-delter)
	(cog-execute! my-mover)
	(show-state my-state)
)

;;; Show the initial state
; (show-state my-state)

;;; Take one step.
;(take-a-step)

;;; Take three steps.
;;; Try it!
;(take-a-step)
;(take-a-step)
;(take-a-step)
