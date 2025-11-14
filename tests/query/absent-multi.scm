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
	(EdgeLink
		(PredicateNode "Agent Mulder")
		(ListLink (VariableNode "$x"))))

(define scully
	(EdgeLink
		(PredicateNode "Agent Scully")
		(ListLink (VariableNode "$y"))))

; Call in the agents
(define call-mulder
	(PutLink
		(EdgeLink
			(PredicateNode "Agent Mulder")
			(VariableNode "$x"))
		(ListLink (ConceptNode "Exploring Area 51"))))

(define call-scully
	(PutLink
		(EdgeLink
			(PredicateNode "Agent Scully")
			(VariableNode "$x"))
		(ListLink (ConceptNode "Late night in the morgue"))))

; Make the agents go away.
(define discredit-mulder
	(QueryLink mulder (DeleteLink mulder)))

(define discredit-scully
	(QueryLink scully (DeleteLink scully)))

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
	(QueryLink
		(AndLink (AbsentLink mulder) (AbsentLink scully))
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-exists)
	)
)

; The UFO is denied if either Mulder or Scully are around.
(define is-invisible
	(QueryLink
		(ChoiceLink mulder scully)
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-denied)
	)
)

; There is undeniable evidence when both of them are working.
(define is-proven
	(QueryLink
		(AndLink mulder scully)
		(PutLink (StateLink ufo-state (VariableNode "$x")) ufo-proven)
	)
)

; Chase key->StateLink->ConceptNode
(define (get-state STATE)
	(filter
		(lambda (CURSTA) (eq? 'ConceptNode (cog-type CURSTA)))
		(map
			(lambda (STALNK) (cog-outgoing-atom STALNK 1))
			(cog-incoming-by-type STATE 'StateLink))))

;; Display the current UFO state
(define (show-ufo-state) (car (get-state ufo-state)))
