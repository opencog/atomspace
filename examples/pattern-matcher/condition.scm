;
; Demonstration of using GroundedPredicateNodes to accept
; or rejct a match (impose a match condition) followed by the
; use of GroudnedSchemaNodes to perform some action, depending
; on whether or not the predicate accepted the match.
;
(use-modules (opencog))
(use-modules (opencog query))

; The function will be used as the condition that will be checked, to
; see if the subsequent action should be taken or not. It returns a
; TruthValue of true or false, depending on whether its argument is the
; ConceptNode "good" or "bad". If it is neither, it throws an error.
(define (truf x)
	(cond
		((equal? x (ConceptNode "good")) (cog-new-stv 1 1))
		((equal? x (ConceptNode "bad")) (cog-new-stv 0 1))
		(else (throw 'whats-up-jack "you done it wrong"))
	)
)

; This defines the action to be taken, if the (pre-)condition holds.
; Its more or less trivial, printing it's argument and then returning
; it wrapped up in an ImplicationLink.
(define (konsekwens x)
	(dsiplay "Taken action on the atom ") (display x) (newline)
	; Must return an atom, or undefined.
	(ImplicationLink x)
)

; Populate the AtomSpace with some data. In this case, two different
; ContextLinks, with two different conditions and actions.  The pattern
; defined later will match this first ContextLink, but not the second.
(ContextLink
	(ConceptNode "situation")
	(EvaluationLink
		(GroundedPredicateNode "scm: truf")
		(ListLink (ConceptNode "good"))
	)
	(ExecutionOutputLink
		(GroundedSchemaNode "scm: konsekwens")
		(ListLink (ConceptNode "acceptance"))
	)
)

; The BindLink will reject this ContextLink.
(ContextLink
	(ConceptNode "predicament")
	(EvaluationLink
		(GroundedPredicateNode "scm: truf")
		(ListLink (ConceptNode "bad"))
	)
	(ExecutionOutputLink
		(GroundedSchemaNode "scm: konsekwens")
		(ListLink (ConceptNode "rejection"))
	)
)

; This pattern will accept one of the two ContextLinks above, and
; reject the other.
(define do-things
	(BindLink
		(VariableList
			(VariableNode "$cxt")
			(VariableNode "$condition")
			(VariableNode "$action")
		)
		(ImplicationLink
			(AndLink
				; If there is a plan to do something ...
				(ContextLink
					(VariableNode "$cxt")
					(VariableNode "$condition")
					(VariableNode "$action")
				)
				; ... and the precondition holds true ...
				(VariableNode "$condition")
			)
			; ...  then perform the action.
			(VariableNode "$action")
		)
	)
)

;; Performing the below should cause only the (ConcpetNode "acceptance")
;; to be printed.
(cog-bind do-things)
