;
; Unit testing for absence of multiple terms
;
; This implements a kind-of Schroedingers-UFO: the UFO exists only
; if Mulder and Scully are not around. If just one is around, its
; existence is denied; if both are around, then there is definitive
; proof.
;
(use-modules (opencog))
(use-modules (opencog exec))

; Clause to match during query - are Agents Mulder and Scully around?
(define mulder
	(EvaluationLink
		(PredicateNode "Agent Mulder")
		(ListLink (VariableNode "$x"))))

(define scully
	(EvaluationLink
		(PredicateNode "Agent Scully")
		(ListLink (VariableNode "$y"))))

; Call in the agents
(define call-mulder
	(PutLink
		(EvaluationLink
			(PredicateNode "Agent Mulder")
			(VariableNode "$x"))
		(ListLink (ConceptNode "Exploring Area 51"))))

(define call-scully
	(PutLink
		(EvaluationLink
			(PredicateNode "Agent Scully")
			(VariableNode "$x"))
		(ListLink (ConceptNode "Late night in the morgue"))))

; Make the agents go away.
(define discredit-mulder
	(BindLink mulder (DeleteLink mulder)))

(define discredit-scully
	(BindLink scully (DeleteLink scully)))

; Status of the UFO
(define ufo-state (AnchorNode "UFO"))
(define ufo-denied (ConceptNode "Government denies knowledge"))
(define ufo-exists (ConceptNode "Located at Area 51"))
(define ufo-proven (ConceptNode "Undeniable evidence for UFO's"))
(define (get-denied) ufo-denied)
(define (get-exists) ufo-exists)
(define (get-proven) ufo-proven)

; Initial state: UFO exists
(StateLink ufo-state ufo-exists)

; The UFO exists only if both Mulder and Scully are not around.
(define is-visible
	(BindLink
		(AndLink (AbsentLink mulder) (AbsentLink scully))
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-exists)
	)
)

; The UFO is denied if either Mulder or Scully are around.
(define is-invisible
	(BindLink
		(ChoiceLink mulder scully)
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-denied)
	)
)

; There is undeniable evidence when both of them are working.
(define is-proven
	(BindLink
		(AndLink mulder scully)
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-proven)
	)
)

;; Display the current UFO state
(define (show-ufo-state)
   (car (cog-chase-link 'StateLink 'ConceptNode ufo-state)))
