;
; condition.scm -- Applying preconditions to actions.
;
; Demonstration of using GroundedPredicateNodes to accept
; or reject a match (impose a match condition) followed by the
; use of GroundedSchemaNodes to perform some action, depending
; on whether or not the predicate accepted the match.
;
(use-modules (opencog) (opencog exec))

; Define a condition that will be checked, to see if a subsequent action
; should be taken or not. It returns a TruthValue of true or false,
; depending on whether its argument is the ConceptNode "good" or "bad".
; If it is neither, it throws an error.
(define (truf x)
	(format #t "Perform condition check on: ~A\n" x)
	(cond
		((equal? x (Concept "good")) (SimpleTruthValue 1 1))
		((equal? x (Concept "bad")) (SimpleTruthValue 0 1))
		(else (throw 'whats-up-jack "you done it wrong"))
	)
)

; Define the action to be taken, if the (pre-)condition holds. The
; action defined here is more or less trivial, printing it's argument
; and then returning it wrapped up in an ImplicationLink.
(define (konsekwens x)
	(format #t "Take action on the atom: ~A\n" x)
	; Must return an atom!
	(Implication x x)
)

; Populate the AtomSpace with some data. In this case, two different
; ContextLinks, with two different conditions and actions.  The pattern
; defined later will match this first ContextLink, but not the second.
(ContextLink
	(Concept "situation")
	(Evaluation
		(GroundedPredicate "scm: truf")
		(List (Concept "good"))
	)
	(ExecutionOutput
		(GroundedSchema "scm: konsekwens")
		(List (Concept "acceptance"))
	)
)

; The BindLink will reject this ContextLink.
(ContextLink
	(Concept "predicament")
	(Evaluation
		(GroundedPredicate "scm: truf")
		(List (Concept "bad"))
	)
	(ExecutionOutput
		(GroundedSchema "scm: konsekwens")
		(List (Concept "rejection"))
	)
)

; This pattern will accept one of the two ContextLinks above, and
; reject the other.
(define do-things
	(Bind
		(VariableList
			(Variable "$cxt")
			(Variable "$condition")
			(Variable "$action")
		)
		(And
			; If there is a plan to do something ...
			(ContextLink
				(Variable "$cxt")
				(Variable "$condition")
				(Variable "$action")
			)
			; ... and the precondition holds true ...
			(Variable "$condition")
		)
		; ...  then perform the action.
		(Variable "$action")
	)
)

;; Performing the below should cause only the (ConceptNode "acceptance")
;; to be printed.
; (cog-execute! do-things)
