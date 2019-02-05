;
; condition.scm -- Applying preconditions to actions.
;
; Demonstration of using GroundedPredicateNodes to accept
; or reject a match (impose a match condition) followed by the
; use of GroudnedSchemaNodes to perform some action, depending
; on whether or not the predicate accepted the match.
;
(use-modules (opencog))

; The function will be used as the condition that will be checked, to
; see if the subsequent action should be taken or not. It returns a
; TruthValue of true or false, depending on whether its argument is the
; ConceptNode "good" or "bad". If it is neither, it throws an error.
(define (truf x)
	(cond
		((equal? x (Concept "good")) (cog-new-stv 1 1))
		((equal? x (Concept "bad")) (cog-new-stv 0 1))
		(else (throw 'whats-up-jack "you done it wrong"))
	)
)

; This defines the action to be taken, if the (pre-)condition holds.
; Its more or less trivial, printing it's argument and then returning
; it wrapped up in an ImplicationLink.
(define (konsekwens x)
	(display "Taken action on the atom ") (display x) (newline)
	; Must return an atom, or undefined.
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
